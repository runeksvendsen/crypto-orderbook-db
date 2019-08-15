{-# LANGUAGE PartialTypeSignatures #-}
module CryptoDepth.OrderBook.Db.Query
()
where

import CryptoDepth.OrderBook.Db.Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Schema.Book   as Book
import qualified CryptoDepth.OrderBook.Db.Schema.Order  as Order
import qualified CryptoDepth.OrderBook.Db               as DB

import Database.Beam
-- import Database.Beam.Backend.SQL
import qualified Database.Beam.Postgres                 as Postgres

-- TMP
import qualified Database.Beam.Backend.SQL.Builder
import Database.Beam.Backend.SQL
import Database.Beam.Query.Internal                     (QNested)
-- TMP

volumeBaseQuote
    ::
    ( BeamSqlBackend be
    )
    => Q be DB.OrderBookDb (QNested s) _
    -> Q be DB.OrderBookDb s
        ( ( PrimaryKey Book.BookT (QGenExpr QValueContext be s)
          , QGenExpr QValueContext be s Bool
          )
        , ( QGenExpr QValueContext be s Double
          , QGenExpr QValueContext be s Double
          )
        )
volumeBaseQuote bookId =
    aggregate_ groupAndSum (orders bookId)
  where
    groupAndSum orders =
        ( ( group_ $ _orderBook orders
          , group_ $ _orderIsBuy orders
          )
        , ( sum_ $ _orderQty orders
          , sum_ $ _orderQty orders **. _orderPrice orders
          )
        )

bestBidAskPrice bookId = do
    bestBuyOrder <-
        limit_ 1 $
        orderBy_ (desc_ . Order._orderPrice) $
        isBuyOrder $
        orders bookId
    bestSellOrder <-
        limit_ 1 $
        orderBy_ (asc_ . Order._orderPrice) $
        isSellOrder $
        orders bookId
    return (Order._orderPrice bestBuyOrder, Order._orderPrice bestSellOrder)
  where
    isSellOrder = filter_ (\o -> Order._orderIsBuy o ==. val_ False)
    isBuyOrder = filter_ (\o -> Order._orderIsBuy o ==. val_ True)

orders bookId = do
    order <- all_ (DB._orders DB.orderBookDb)
    guard_ (Order._orderBook order `references_` bookId)
    return order
