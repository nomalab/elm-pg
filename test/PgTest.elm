port module PgTest exposing (..)

import Task

import Ordeal exposing (..)

import Pg.Query as Query exposing (Query)
import Pg.Pool as Pool exposing (Pool)
import Pg.Client as Client exposing (Client)

main: Ordeal
main = run emit tests

port emit : Event -> Cmd msg

tests: Test
tests =
  describe "Pg"
    [ pool
    ]

clientSettings: Client.Settings
clientSettings =
  { user = Just "elm-pg"
  , password = Just "elm-pg-password"
  , database = Just "elm-pg"
  , port_ = Just 5432
  , connectionString = Nothing
  , ssl = Nothing
  }

localPool: Pool
localPool =
  Pool.create
    clientSettings
    { max = Just 10
    , connectionTimeoutMillis = Nothing
    , idleTimeoutMillis = Nothing
    }

initQuery: Query
initQuery = Query.create
  """
  DROP SCHEMA public CASCADE;
  CREATE SCHEMA public;
  CREATE TABLE test (
    id UUID PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER
  )
  """
  []

wrongQuery: Query
wrongQuery = Query.create "SELECT wrong_column FROM test" []

testPool: Pool -> Int -> Int -> Int -> Expectation
testPool pool t i w =
  all
  [ Pool.totalCount pool |> shouldEqual t
  , Pool.idleCount pool |> shouldEqual i
  , Pool.waitingCount pool |> shouldEqual w
  ]

pool: Test
pool =
  describe "Pool"
    [ test "equals" (
      Pool.equals localPool localPool |> shouldEqual True
    )
    , test "connect" (
      Pool.connect localPool
      |> shouldSucceedAnd (\client ->
        (testPool localPool 1 0 0)
        |> shouldSucceedAnd (\_ ->
          Client.release client |> shouldSucceedAnd (\_ -> testPool localPool 1 1 0 )
        )
      )
    )
    , test "multi-connect" (
      [ Pool.connect localPool, Pool.connect localPool, Pool.connect localPool ]
      |> Task.sequence
      |> shouldSucceedAnd (\clients ->
        let
          (c1, c2, c3) = case clients of
            c1 :: c2 :: c3 :: [] -> (c1, c2, c3)
            _ -> Debug.crash "We have 3 clients"
        in
          testPool localPool 3 0 0
          |> shouldSucceedAnd (\_ ->
            Client.release c1 |> shouldSucceedAnd (\_ -> testPool localPool 3 1 0)
          )
          |> shouldSucceedAnd (\_ ->
            Client.release c2 |> shouldSucceedAnd (\_ -> testPool localPool 3 2 0)
          )
          |> shouldSucceedAnd (\_ ->
            Client.release c3 |> shouldSucceedAnd (\_ -> testPool localPool 3 3 0)
          )
      )
    )
    -- , test "over-connect" (
    --   [ Pool.connect localPool, Pool.connect localPool, Pool.connect localPool
    --   , Pool.connect localPool, Pool.connect localPool, Pool.connect localPool
    --   , Pool.connect localPool, Pool.connect localPool, Pool.connect localPool
    --   , Pool.connect localPool, Pool.connect localPool, Pool.connect localPool
    --   ]
    --   |> Task.sequence
    --   |> andTest (\clients ->
    --     testPool localPool 10 0 2
    --       -- |> andThen (\_ -> Client.release c1 |> andTest (\_ -> testPool localPool 3 1 0))
    --       -- |> andThen (\_ -> Client.release c2 |> andTest (\_ -> testPool localPool 3 2 0))
    --       -- |> andThen (\_ -> Client.release c3 |> andTest (\_ -> testPool localPool 3 3 0))
    --   )
    -- )
    , test "query" (
      Pool.query initQuery localPool
      |> shouldSucceed
    )
    , test "wrong query" (
      Pool.query wrongQuery localPool
      |> shouldFailAnd (\err -> case err of
        Query.Error _ -> failure "Should be a query error"
        Query.WrongDecoder _ -> failure "Should be a query error"
        Query.WrongQuery e ->
          all
          [ e.severity |> shouldEqual "ERROR"
          , e.code |> shouldEqual "42703"
          , e.message |> shouldEqual "column \"wrong_column\" does not exist"
          , e.position |> shouldEqual (Just "8")
          , List.length e.stack |> shouldBeGreaterThan 1
          ]
      )
    )
    , test "end" (
      Pool.end localPool
      |> shouldSucceed
    )
    , test "ended" (
      Pool.connect localPool
      |> shouldFailAnd (\error ->
        error.message |> shouldEqual "Cannot use a pool after calling end on the pool"
      )
    )
    ]
