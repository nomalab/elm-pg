module Pg.Params exposing (..)

import Json.Encode as Encode
import Date exposing (Date, Month(..))

type Param
  = NullParam
  | IntParam Int
  | FloatParam Float
  | StringParam String
  | BoolParam Bool
  | DateParam Date
  | MaybeParam (Maybe Param)
  | JsonParam Encode.Value
  | ListParam (List Param)
  | RawParam Encode.Value

nowUtc: String
nowUtc =
  "(now() at time zone 'utc')"

null: Param
null = NullParam

boolean: Bool -> Param
boolean = BoolParam

integer: Int -> Param
integer = IntParam

real: Float -> Param
real = FloatParam

double: Float -> Param
double = FloatParam

varchar: String -> Param
varchar = StringParam

text: String -> Param
text = StringParam

uuid: String -> Param
uuid = StringParam

date: Date -> Param
date = DateParam

maybe: (a -> Param) -> Maybe a -> Param
maybe fn m = MaybeParam (Maybe.map fn m)

list: (a -> Param) -> List a -> Param
list mapper list = ListParam (List.map mapper list)

jsonb: Encode.Value -> Param
jsonb = JsonParam

raw: Encode.Value -> Param
raw = RawParam

orNull: Maybe Param -> Param
orNull mParam =
  Maybe.withDefault null mParam

prependZeros: Int -> String -> String
prependZeros length str =
  if String.length str < length
  then prependZeros (length - 1) ("0" ++ str)
  else str

encodeMonth: Month -> String
encodeMonth month =
  case month of
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    Apr -> "04"
    May -> "05"
    Jun -> "06"
    Jul -> "07"
    Aug -> "08"
    Sep -> "09"
    Oct -> "10"
    Nov -> "11"
    Dec -> "12"

encodeDate: Date -> Encode.Value
encodeDate date =
  Encode.string <|
    (Date.year date |> toString |> prependZeros 4) ++ "-" ++
    (Date.month date |> encodeMonth) ++ "-" ++
    (Date.day date |> toString |> prependZeros 2) ++ "T" ++
    (Date.hour date |> toString |> prependZeros 2) ++ ":" ++
    (Date.minute date |> toString |> prependZeros 2) ++ ":" ++
    (Date.second date |> toString |> prependZeros 2) ++ "." ++
    (Date.millisecond date |> toString |> prependZeros 3) ++ "Z"

encode: Param -> Encode.Value
encode param =
  case param of
    NullParam -> Encode.null
    IntParam i -> Encode.int i
    FloatParam f -> Encode.float f
    StringParam s -> Encode.string s
    BoolParam b -> Encode.bool b
    DateParam d -> encodeDate d
    MaybeParam mParam -> Maybe.withDefault (Encode.null) (Maybe.map encode mParam)
    ListParam list -> Encode.list (List.map encode list)
    JsonParam p -> Encode.encode 0 p |> Encode.string
    RawParam r -> r

encodeList: List Param -> Encode.Value
encodeList params =
  Encode.list <| List.map encode params
