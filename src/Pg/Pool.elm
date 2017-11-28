effect module Pg.Pool where { subscription = PoolSub } exposing (..)

import Task exposing (Task)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)

import Error exposing (Error)
import Kernel.Helpers

import Pg.Query as Query exposing (Query)
import Pg.Client as Client exposing (Client)
import Native.Pg

type Pool = Pool

type alias Settings =
  { max: Maybe Int
  , connectionTimeoutMillis: Maybe Int
  , idleTimeoutMillis: Maybe Int
  }

create: Client.Settings -> Settings -> Pool
create clientSettings settings =
  Native.Pg.poolCreate (Client.encodeSettings clientSettings) (encodeSettings settings)

connect: Pool -> Task Error Client
connect pool =
  Native.Pg.poolConnect pool
  |> Task.mapError Error.parse

query: Query -> Pool -> Task Query.Error Query.Result
query qry pool =
  Native.Pg.poolQuery (Query.encode qry) pool
  |> Task.mapError Query.decodeError

typedQuery: Decoder a -> Query -> Pool -> Task Query.Error (List a)
typedQuery decoder qry pool =
  query qry pool
  |> Task.andThen (\result ->
    case Decode.decodeValue (Decode.list decoder) result.rows of
      Ok rows -> Task.succeed rows
      Err err -> Task.fail (Query.WrongDecoder err)
  )

end: Pool -> Task Error ()
end pool =
  Native.Pg.poolEnd pool
  |> Task.mapError Error.parse

totalCount: Pool -> Int
totalCount =
  Native.Pg.poolTotalCount

idleCount: Pool -> Int
idleCount =
  Native.Pg.poolIdleCount

waitingCount: Pool -> Int
waitingCount =
  Native.Pg.poolWaitingCount

equals: Pool -> Pool -> Bool
equals =
  Native.Pg.equals

encodeSettings: Settings -> Encode.Value
encodeSettings settings =
  [ settings.max |> Maybe.map (\max -> ("max", Encode.int max))
  , settings.connectionTimeoutMillis |> Maybe.map (\connectionTimeoutMillis -> ("connectionTimeoutMillis", Encode.int connectionTimeoutMillis))
  , settings.idleTimeoutMillis |> Maybe.map (\idleTimeoutMillis -> ("idleTimeoutMillis", Encode.int idleTimeoutMillis))
  ]
  |> List.filterMap identity
  |> Encode.object


-- -----------------------------------------------------------------------------
-- Effects

type Msg
  = Connect { pool: Pool, client: Client }
  | Acquire { pool: Pool, client: Client }
  | Error   { pool: Pool, client: Client, error: Error }
  | Remove  { pool: Pool, client: Client }

type PoolSub msg
  = Listen (Msg -> msg)

listen: (Msg -> msg) -> Sub msg
listen =
  subscription << Listen

subMap : (a -> b) -> PoolSub a -> PoolSub b
subMap f sub =
  case sub of
    Listen tagger -> Listen (tagger >> f)

type alias State msg =
  { initialized: Bool
  , subs: List (PoolSub msg)
  }

init : Task Never (State msg)
init =
  Native.Pg.poolInit
    { connect = Connect
    , acquire = Acquire
    , error = Error
    , remove = Remove
    }
  |> Task.map (\_ -> { initialized = False, subs = [] })

onEffects
  : Platform.Router msg Msg
  -> List (PoolSub msg)
  -> State msg
  -> Task Never (State msg)
onEffects router subs state =
  (
    if state.initialized
    then Task.succeed state
    else
      Native.Pg.poolSetup (Platform.sendToSelf router)
      |> Task.map (\_ -> { state | initialized = True })
  )
  |> Task.map (\s -> { s | subs = subs })

onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
  state.subs
  |> List.map (\sub -> case sub of
    Listen tagger -> Platform.sendToApp router (tagger selfMsg)
  )
  |> Task.sequence
  |> Task.map (\_ -> state)

noWarnings: String
noWarnings = Kernel.Helpers.noWarnings
