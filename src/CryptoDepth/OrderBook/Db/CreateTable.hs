{-# LANGUAGE OverloadedStrings #-}
module CryptoDepth.OrderBook.Db.CreateTable
( createTables
, dropTables
, DB.OrderBookDb
-- * Re-exports
, PG.Postgres
)
where

import CryptoDepth.OrderBook.Db.Schema.Book         -- (BookT(Book), BookId(BookId))
import CryptoDepth.OrderBook.Db.Schema.Order        (OrderT(Order))
import qualified CryptoDepth.OrderBook.Db           as DB

import qualified Database.Beam                      as B
import           Database.Beam.Migrate.SQL.Tables   (field, notNull)
import qualified Database.Beam.Migrate.SQL.Tables   as BM
import qualified Database.Beam.Migrate.Types        as BM
import qualified Database.Beam.Postgres             as PG


dropTables
    :: BM.CheckedDatabaseSettings PG.Postgres DB.OrderBookDb
    -> BM.Migration PG.Postgres ()
dropTables (DB.OrderBookDb books orders) = do
    BM.dropTable orders
    BM.dropTable books

createTables
    :: ()
    -> BM.Migration PG.Postgres (BM.CheckedDatabaseSettings PG.Postgres DB.OrderBookDb)
createTables () =
    DB.OrderBookDb
        <$> bookTable
        <*> orderTable

bookTable
    :: BM.Migration PG.Postgres
        (BM.CheckedDatabaseEntity PG.Postgres DB.OrderBookDb (B.TableEntity BookT))
bookTable =
    BM.createTable "books" $ Book
        (field "id" PG.serial BM.unique notNull)
        (field "time" B.timestamp notNull)
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
