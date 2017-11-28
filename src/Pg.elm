module Libs.Pg exposing (..)
--
import Dict exposing (Dict)
import Json.Encode as Encode
import Task as Task exposing (Task)
import Date as Date exposing (Date)

import Kernel.Helpers
import Native.Pg


-- -----------------------------------------------------------------------------
-- Plannification

type Transaction = Yes | No

type alias Plan err res =
  { queries: Client -> Task err res
  , isTransaction: Transaction
  }

doQuery: Request a -> Client -> Response a
doQuery request client =
  request client
  |> Task.andSide (\_ -> release client)
  |> Task.onError (\err ->
    release client
    |> Task.andThen (\_ -> Task.fail err)
  )

attempt: Pool -> Query a -> Response a
attempt pool query =
  connect pool
  |> Task.andThen (case query.transaction of
    No -> doQuery query.request
    Yes -> doTransaction query.request
  )

perform: Pool -> Query a -> Response a
perform pool query =
  connect pool
  |> Task.andThen (case query.transaction of
    No -> doQuery query.request
    Yes -> doTransaction query.request
  )

--
-- -- -----------------------------------------------------------------------------
-- -- Querying
--
-- query: String -> List Param -> QueryRaw
-- query sql params client =
--   Native.Pg.query sql params (encodeParams params)
--

--
-- doQuery: Request a -> Client -> Response a
-- doQuery request client =
--   request client
--   |> Task.andSide (\_ -> release client)
--   |> Task.onError (\err ->
--     release client
--     |> Task.andThen (\_ -> Task.fail err)
--   )
--
-- begin: QueryRaw
-- begin client =
--   query "BEGIN" [] client
--
-- commit: QueryRaw
-- commit client =
--   query "COMMIT" [] client
--
-- rollback: QueryRaw
-- rollback client =
--   query "ROLLBACK" [] client
--
-- doTransaction: Request a -> Client -> Response a
-- doTransaction request client =
--   -- Begin the transaction
--   begin client
--   |> Task.andThen (\_ ->
--     request client
--     -- If all good, commit the transaction
--     -- and release the client
--     |> Task.andSide (\_ ->
--       commit client
--       |> Task.andThen release
--     )
--     -- If failed
--     |> Task.onError (\err1 ->
--       -- Try to rollback the transaction
--       rollback client
--       -- If rollback failed, this is critical but still, try to release client
--       |> Task.onError (\err2 ->
--         release client
--         |> Task.andThen (\_ ->
--           Task.fail err2
--         )
--       )
--       -- If rollback done, also release the client
--       |> Task.andThen release
--       -- But still, the task is a failure
--       |> Task.andThen (\_ ->
--         Task.fail err1
--       )
--     )
--   )
--
-- merge: Transaction -> Transaction -> Transaction
-- merge trans1 trans2 =
--   if trans1 == No
--   then trans2
--   else trans1
--
-- mergeSequence: List Transaction -> Transaction
-- mergeSequence =
--   List.foldl merge
--
-- -- -----------------------------------------------------------------------------
-- -- Helpers
--
-- map: (a -> b) -> Query a -> Query b
-- map mapper qry =
--   { qry | request = (\client -> qry.request client |> Task.map mapper) }
--
-- sequence: List (Query a) -> Query (List a)
-- sequence queries =
--   case queries of
--     [] -> { request = (\_ -> Task.succeed []), transaction = No }
--     qry :: [] -> { qry | request = (\client -> qry.request client |> Task.map (\res -> [ res ])), transaction = qry.transaction }
--     first :: rest -> List.foldl andThen first rest
--
-- andThen: (a -> Query b) -> Query a -> Query b
-- andThen next qry =
--   { request = (\client -> qry.request client |> Task.andThen next |> Task.map .request)
--   , transaction = Yes
--   }
--
-- andThenWithoutTransaction: (a -> Query b) -> Query a -> Query b
-- andThenWithoutTransaction next qry =
--   { request = (\client -> qry.request client |> Task.andThen next |> Task.map .request)
--   , transaction = No
--   }
--
-- andSide: (a -> Query b) -> Query a -> Query a
-- andSide fn qry client =
--   qry client
--   |> Task.andSide (\res -> fn res client)
--
-- andTask: (a -> Task Error b) -> Query a -> Query b
-- andTask next qry =
--   { qry | request = (\client -> qry.request client |> Task.andThen next) }
--
-- andUnprefix2: Prefixed a -> Prefixed b -> Query (QueryResult RowValue) -> Query (QueryResult (a, b))
-- andUnprefix2 pre1 pre2 qry client =
--   qry client
--   |> Task.map (unprefix2 pre1 pre2)
--   |> Task.andThen Task.fromResult
--
-- andUnprefix3: Prefixed a -> Prefixed b -> Prefixed c -> Query (QueryResult RowValue) -> Query (QueryResult (a, b, c))
-- andUnprefix3 pre1 pre2 pre3 qry client =
--   qry client
--   |> Task.map (unprefix3 pre1 pre2 pre3)
--   |> Task.andThen Task.fromResult
--
-- andUnprefix4: Prefixed a -> Prefixed b -> Prefixed c  -> Prefixed d -> Query (QueryResult RowValue) -> Query (QueryResult (a, b, c, d))
-- andUnprefix4 pre1 pre2 pre3 pre4 qry client =
--   qry client
--   |> Task.map (unprefix4 pre1 pre2 pre3 pre4)
--   |> Task.andThen Task.fromResult
--
-- andUnprefix7: Prefixed a -> Prefixed b -> Prefixed c  -> Prefixed d -> Prefixed e -> Prefixed f -> Prefixed g -> Query (QueryResult RowValue) -> Query (QueryResult (a, b, c, d, e, f, g))
-- andUnprefix7 pre1 pre2 pre3 pre4 pre5 pre6 pre7 qry client =
--   qry client
--   |> Task.map (unprefix7 pre1 pre2 pre3 pre4 pre5 pre6 pre7)
--   |> Task.andThen Task.fromResult
--
--
-- succeed: a -> Query a
-- succeed result =
--   fromTask (Task.succeed result)
--
-- fail: Error -> Query a
-- fail error =
--   fromTask (Task.fail error)
--
-- fromTask: Task Error a -> Query a
-- fromTask task =
--   { request = (\_ -> task)
--   , transaction = No
--   }
--
-- fromResult: Result Error a -> Query a
-- fromResult res =
--   case res of
--     Ok value -> succeed value
--     Err err  -> fail err
--
-- isTransaction: Query a -> Query a
-- isTransaction qry =
--   { qry | transaction = Yes }
--
--
-- -- Typed queries
-- decodeRows: Decoder a -> (QueryResult RowValue) -> (Result Error (QueryResult a))
-- decodeRows decoder result =
--   let
--     rows = Encode.list result.rows
--   in
--     Data.fromJS (Data.list data) rows
--     |> Result.map (\parsedRows -> { result | rows = parsedRows })
--     |> Result.mapError (\err -> Error.db |> Error.withParseError rows err) -- Result Error (QueryResult a)
--
-- decodeQuery: Data a -> QueryResult RowValue -> Task Error (QueryResult a)
-- decodeQuery data result =
--   decodeRows data result -- Result String (QueryResult a)
--   |> Task.fromResult -- Task Error (QueryResult a)
--
-- typedQuery: Data a -> String -> List Param -> Query (QueryResult a)
-- typedQuery data sql params =
--   query sql params -- Task Error (QueryResult RowValue)
--   |> andTask (decodeQuery data) -- Task Error (QueryResult a)
--
-- typedInsert: Data a -> String -> List Param -> Query a
-- typedInsert client data sql params =
--   typedQuery client data sql params
--   |> Task.andThen (\result -> case result.rows of
--     value :: [] ->
--       Task.succeed value
--     _ ->
--       Task.fail (Error.db |> Error.withMessage ("Expected exactly one item but got " ++ (toString <| List.length result.rows)))
--   )
--
-- typedQueryOne: Data a -> String -> List Param -> Query (Maybe a)
-- typedQueryOne client data sql params =
--   typedQuery client data sql params
--   |> Task.andThen (\result -> case result.rows of
--     [] ->
--       Task.succeed Nothing
--     value :: [] ->
--       Task.succeed (Just value)
--     _ ->
--       Task.fail (Error.db |> Error.withMessage ("Expected at most one item but got " ++ (toString <| List.length result.rows)))
--   )
--
-- typedQueryList: Data a -> String -> List Param -> Query (List a)
-- typedQueryList client data sql params =
--   typedQuery client data sql params
--   |> Task.map (\r -> r.rows)
--
--
-- -- JOINS
--
-- prefixField: String -> String -> String -> String
-- prefixField table pre field =
--   table ++ ".\"" ++ field ++ "\" as \"" ++ pre ++ field ++ "\""
--
-- prefix: String -> String -> Data a -> String
-- prefix table pre data =
--   Data.keys data
--   |> List.map (prefixField table pre)
--   |> String.join ", "
--
-- unprefix: String -> RowValue -> RowValue
-- unprefix =
--   Native.Pg.unprefix
--
-- unprefix2: Prefixed a -> Prefixed b -> QueryResult RowValue -> Result Error (QueryResult (a, b))
-- unprefix2 (pre1, data1) (pre2, data2) result =
--   let
--     parsedResult = { result | rows = List.map (\row -> Json.Encode.list [unprefix pre1 row, unprefix pre2 row]) result.rows }
--     data = Data.tuple2 data1 data2
--   in
--     Data.fromJS (Data.list data) (Json.Encode.list parsedResult.rows)
--     |> Result.map (\rows -> { parsedResult | rows = rows })
--     |> Result.mapError (\err -> Error.db |> Error.withParseError parsedResult.rows err)
--
-- unprefix3: Prefixed a -> Prefixed b -> Prefixed c -> QueryResult RowValue -> Result Error (QueryResult (a, b, c))
-- unprefix3 (pre1, data1) (pre2, data2) (pre3, data3) result =
--   let
--     parsedResult = { result | rows = List.map (\row -> Json.Encode.list [unprefix pre1 row, unprefix pre2 row, unprefix pre3 row]) result.rows }
--     data = Data.tuple3 data1 data2 data3
--   in
--     Data.fromJS (Data.list data) (Json.Encode.list parsedResult.rows)
--     |> Result.map (\rows -> { parsedResult | rows = rows })
--     |> Result.mapError (\err -> Error.db |> Error.withParseError parsedResult.rows err)
--
-- unprefix4: Prefixed a -> Prefixed b -> Prefixed c -> Prefixed d -> QueryResult RowValue -> Result Error (QueryResult (a, b, c, d))
-- unprefix4 (pre1, data1) (pre2, data2) (pre3, data3) (pre4, data4) result =
--   let
--     parsedResult = { result | rows = List.map (\row -> Json.Encode.list [unprefix pre1 row, unprefix pre2 row, unprefix pre3 row, unprefix pre4 row]) result.rows }
--     data = Data.tuple4 data1 data2 data3 data4
--   in
--     Data.fromJS (Data.list data) (Json.Encode.list parsedResult.rows)
--     |> Result.map (\rows -> { parsedResult | rows = rows })
--     |> Result.mapError (\err -> Error.db |> Error.withParseError parsedResult.rows err)
--
-- unprefix7: Prefixed a -> Prefixed b -> Prefixed c -> Prefixed d -> Prefixed e -> Prefixed f -> Prefixed g -> QueryResult RowValue -> Result Error (QueryResult (a, b, c, d, e, f, g))
-- unprefix7 (pre1, data1) (pre2, data2) (pre3, data3) (pre4, data4) (pre5, data5) (pre6, data6) (pre7, data7) result =
--   let
--     parsedResult = { result | rows = List.map (\row -> Json.Encode.list [unprefix pre1 row, unprefix pre2 row, unprefix pre3 row, unprefix pre4 row, unprefix pre5 row, unprefix pre6 row, unprefix pre7 row]) result.rows }
--     data = Data.tuple7 data1 data2 data3 data4 data5 data6 data7
--   in
--     Data.fromJS (Data.list data) (Json.Encode.list parsedResult.rows)
--     |> Result.map (\rows -> { parsedResult | rows = rows })
--     |> Result.mapError (\err -> Error.db |> Error.withParseError parsedResult.rows err)
--
--
-- -- REFERENCE STUFF
-- paramsReference: Referenced a -> List Param
-- paramsReference ref =
--   [ uuid ref.id
--   , uuid ref.creator
--   , Date ref.createdAt
--   , uuid ref.updater
--   , Date ref.updatedAt
--   ]
--
-- noWarnings: String
-- noWarnings = Kernel.Helpers.noWarnings
