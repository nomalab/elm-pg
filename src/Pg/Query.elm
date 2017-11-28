module Pg.Query exposing
  ( Query
  , Result
  , Field
  , Error(..)
  , create
  , prepare
  , asArray
  , asObject
  , encode
  , decodeError
  )

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Error

import Pg.Params as Params exposing (Param)

-- -----------------------------------------------------------------------------
-- Querying

type RowMode = RowModeObject | RowModeArray

type alias Query =
  { name: Maybe String
  , sql: String
  , params: List Param
  , rowMode: RowMode
  }

type alias Result =
  { command: String
  , rowCount: Int
  , fields: List Field
  , rows: Encode.Value
  -- , rows: List Encode.Value
  , rowAsArray: Bool
  }

type alias Field =
  { name: String
  , tableID: Int
  , columnID: Int
  , dataTypeID: Int
  , dataTypeSize: Int
  , dataTypeModifier: Int
  , format: String
  }

type alias QueryError =
  { severity: String
  , code: String
  , position: Maybe String
  , message: String
  , stack: List String
  -- , messageDetail: Maybe String
  -- , messageHint: Maybe String
  }

type Error
  = Error Error.Error
  | WrongQuery QueryError
  | WrongDecoder String

create: String -> List Param -> Query
create sql params =
  { name = Nothing
  , sql = sql
  , params = params
  , rowMode = RowModeObject
  }

prepare: String -> String -> List Param -> Query
prepare name sql params =
  create sql params
  |> withName name

withName: String -> Query -> Query
withName name qry =
  { qry | name = Just name }

asArray: Query -> Query
asArray qry =
  { qry | rowMode = RowModeArray }

asObject: Query -> Query
asObject qry =
  { qry | rowMode = RowModeObject }


-- -----------------------------------------------------------------------------
-- Useful queries

begin: Query
begin =
  create "BEGIN" []

commit: Query
commit =
  create "COMMIT" []

rollback: Query
rollback =
  create "ROLLBACK" []


-- -----------------------------------------------------------------------------
-- JSON

encode: Query -> Encode.Value
encode qry =
  [ qry.name |> Maybe.map (\name -> ("name", Encode.string name))
  , Just ("text", Encode.string qry.sql)
  , Just ("values", Params.encodeList qry.params)
  , case qry.rowMode of
      RowModeObject -> Nothing
      RowModeArray -> Just ("rowMode", Encode.string "array")
  ]
  |> List.filterMap identity
  |> Encode.object

decodeError: Encode.Value -> Error
decodeError value =
  case Decode.decodeValue decoderQueryError value of
    Ok err -> WrongQuery err
    Err _ -> Error (Error.parse value)

decoderStack: Decoder (List String)
decoderStack =
  Decode.field "stack" Decode.string
  |> Decode.map (String.split "\n")
  |> Decode.map (List.map String.trim)

decoderQueryError: Decoder QueryError
decoderQueryError =
  Decode.map5 QueryError
    (Decode.field "severity" Decode.string)
    (Decode.field "code" Decode.string)
    (Decode.maybe <| Decode.field "position" Decode.string)
    (Decode.field "message" Decode.string)
    (decoderStack)
