{-# LANGUAGE OverloadedStrings #-}
module CryptoDepth.OrderBook.Db.CreateTable
( createTables
, dropTables
, DB.OrderBookDb
-- * Re-exports
, PG.Postgres
)
where

import CryptoDepth.OrderBook.Db.Schema.Run          (RunT(Run))
import CryptoDepth.OrderBook.Db.Schema.Book
import CryptoDepth.OrderBook.Db.Schema.Order        (OrderT(Order))
import qualified CryptoDepth.OrderBook.Db.Database           as DB

import qualified Database.Beam                      as B
import qualified Database.Beam.Backend.SQL          as B
import           Database.Beam.Migrate.SQL.Tables   (field, notNull)
import qualified Database.Beam.Migrate.SQL.Tables   as BM
import qualified Database.Beam.Migrate.Types        as BM
import qualified Database.Beam.Postgres             as PG


dropTables
    :: BM.CheckedDatabaseSettings PG.Postgres DB.OrderBookDb
    -> BM.Migration PG.Postgres ()
dropTables (DB.OrderBookDb runs books orders) = do
    BM.dropTable orders
    BM.dropTable books
    BM.dropTable runs

createTables
    :: ()
    -> BM.Migration PG.Postgres (BM.CheckedDatabaseSettings PG.Postgres DB.OrderBookDb)
createTables () =
    DB.OrderBookDb
        <$> runTable
        <*> bookTable
        <*> orderTable

utcTimestamp :: B.BeamSqlBackend be => B.DataType be UTCTime
utcTimestamp = B.DataType (B.timestampType Nothing True)

runTable
    :: BM.Migration PG.Postgres
        (BM.CheckedDatabaseEntity PG.Postgres DB.OrderBookDb (B.TableEntity RunT))
runTable =
    BM.createTable "runs" $ Run
        (field "id" PG.serial BM.unique notNull)
        (field "time_start" utcTimestamp notNull)
        (field "time_end" utcTimestamp notNull)

bookTable
    :: BM.Migration PG.Postgres
        (BM.CheckedDatabaseEntity PG.Postgres DB.OrderBookDb (B.TableEntity BookT))
bookTable =
    BM.createTable "books" $ Book
        (field "id" PG.serial BM.unique notNull)
        (RunId (field "run__id" B.int))
        (field "time" utcTimestamp notNull)
        (field "venue" (B.varchar Nothing) notNull)
        (field "base" (B.varchar Nothing) notNull)
        (field "quote" (B.varchar Nothing) notNull)

orderTable
    :: BM.Migration PG.Postgres
        (BM.CheckedDatabaseEntity PG.Postgres DB.OrderBookDb (B.TableEntity OrderT))
orderTable =
    BM.createTable "orders" $ Order
        (BookId (field "book__id" B.int))
        (field "is_buy" B.boolean notNull)
        (field "qty" B.double notNull)
        (field "price" B.double notNull)
