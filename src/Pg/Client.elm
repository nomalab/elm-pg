effect module Pg.Client where { subscription = ClientSub } exposing (..)

import Task exposing (Task)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)

import Error exposing (Error)
import Kernel.Helpers

import Pg.Query as Query exposing (Query)
import Native.Pg

type Client = Client

type alias Settings =
  { user: Maybe String
  , password: Maybe String
  , database: Maybe String
  , port_: Maybe Int
  , connectionString: Maybe String
  , ssl: Maybe Encode.Value
  }

create: Settings -> Client
create settings =
  Native.Pg.clientCreate (encodeSettings settings)

connect: Client -> Task Error ()
connect client =
  Native.Pg.clientConnect client
  |> Task.mapError Error.parse

release: Client -> Task Error ()
release client =
  Native.Pg.clientRelease client
  |> Task.mapError Error.parse

end: Client -> Task Error ()
end client =
  Native.Pg.clientEnd
  |> Task.mapError Error.parse

query: Query -> Client -> Task Query.Error Query.Result
query qry client =
  Native.Pg.clientQuery (Query.encode qry) client
  |> Task.mapError Query.decodeError

typedQuery: Decoder a -> Query -> Client -> Task Query.Error (List a)
typedQuery decoder qry client =
  query qry client
  |> Task.andThen (\result ->
    case Decode.decodeValue (Decode.list decoder) result.rows of
      Ok rows -> Task.succeed rows
      Err err -> Task.fail (Query.WrongDecoder err)
  )

equals: Client -> Client -> Bool
equals =
  Native.Pg.equals

encodeSettings: Settings -> Encode.Value
encodeSettings settings =
  [ settings.user |> Maybe.map (\user -> ("user", Encode.string user))
  , settings.password |> Maybe.map (\password -> ("password", Encode.string password))
  , settings.database |> Maybe.map (\database -> ("database", Encode.string database))
  , settings.port_ |> Maybe.map (\port_ -> ("port", Encode.int port_))
  , settings.connectionString |> Maybe.map (\connectionString -> ("connectionString", Encode.string connectionString))
  , settings.ssl |> Maybe.map (\ssl -> ("ssl", ssl))
  ]
  |> List.filterMap identity
  |> Encode.object

-- -----------------------------------------------------------------------------
-- Effects

type Msg
  = Error { client: Client, error: Error }
  | End { client: Client }
  | Notification { client: Client, notification: NotificationValue }
  | Notice { client: Client, notice: String }

type alias NotificationValue =
  { processId: Int
  , channel: String
  , payload: Maybe String
  }

type ClientSub msg
  = Listen (Msg -> msg)

listen: (Msg -> msg) -> Sub msg
listen =
  subscription << Listen

subMap : (a -> b) -> ClientSub a -> ClientSub b
subMap f sub =
  case sub of
    Listen tagger -> Listen (tagger >> f)

type alias State msg =
  { initialized: Bool
  , subs: List (ClientSub msg)
  }

init : Task Never (State msg)
init =
  Native.Pg.clientInit
    { error = Error
    , end = End
    , notification = Notification
    , notice = Notice
    }
  |> Task.map (\_ -> { initialized = False, subs = [] })

onEffects
  : Platform.Router msg Msg
  -> List (ClientSub msg)
  -> State msg
  -> Task Never (State msg)
onEffects router subs state =
  (
    if state.initialized
    then Task.succeed state
    else
      Native.Pg.clientSetup (Platform.sendToSelf router)
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
