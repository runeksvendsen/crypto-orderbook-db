-- {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CryptoDepth.OrderBook.Db.Query
( buySellPriceRange
, volumeBaseQuote
)
where

import qualified CryptoDepth.OrderBook.Db.Schema.Order    as Order
import qualified CryptoDepth.OrderBook.Db.Schema.Book     as Book
import qualified CryptoDepth.OrderBook.Db.Database        as DB

import Database.Beam.Query
-- DEBUG
import Database.Beam                          (C)
import qualified Database.Beam.Backend.SQL
-- import qualified Database.Beam.Query.Internal
import           Database.Beam.Query.Internal
import qualified Database.Beam.Schema.Tables
import qualified Data.Text


-- | Total volume of sell orders and buy orders, respectively,
--    measured in both base currency and quote currency.
--   Output: (book, base_volume, quote_volume)
volumeBaseQuote book = do
    let orders = DB.bookOrders book
    (book, aggregate_ groupAndSum orders)
  where
    groupAndSum orders =
        ( ( group_ $ Order.orderBook orders
          , group_ $ Order.orderIsBuy orders
          )
        , ( sum_ $ Order.orderQty orders
          , sum_ $ Order.orderQty orders **. Order.orderPrice orders
          )
        )


testQuery = do
    bookOrders <- allBookOrders
    buySellPriceRange bookOrders


-- | Price range of buy orders and sell orders, respectively.
--   Output:
--      ( (base, quote)
--      , (highest_buy_order_price, lowest_buy_order_price)
--      , (highest_sell_order_price, lowest_sell_order_price)
--      )
--  Can be used to calculate:
--      a) Spread (lowest_sell_order_price - highest_buy_order_price)
--      b) Sell/buy order price range:
--          percentage difference between best-priced and worst-priced order

-- buySellPriceRange ::
--     (HasSqlEqualityCheck be Book.Word32,
--       HasSqlEqualityCheck be Bool)
--     => Book.BookT (QExpr be (QNested (QNested s)))
--     -> Q be DB.OrderBookDb s
--           ((QGenExpr QValueContext be (QNested (QNested s)) Data.Text.Text,
--             QGenExpr QValueContext be (QNested (QNested s)) Data.Text.Text),
--             (QGenExpr QValueContext be s Double,
--             QGenExpr QValueContext be s Double),
--             (QGenExpr QValueContext be s Double,
--             QGenExpr QValueContext be s Double))
buySellPriceRange (book, orders) = do
    -- let orders = DB.bookOrders book
    highestPricedBid <- firstOrderSortedBy orders True desc_
    lowestPricedBid <- firstOrderSortedBy orders True asc_
    highestPricedAsk <- firstOrderSortedBy orders False desc_
    lowestPricedAsk <- firstOrderSortedBy orders False asc_
    return
        ( (Book.bookBase book, Book.bookQuote book)
        , (price highestPricedBid, price lowestPricedBid)
        , (price highestPricedAsk, price lowestPricedAsk)
        )
  where
    price = Order.orderPrice

{-

SELECT book.base, book.quote, ... FROM
    books book JOIN orders order ON book.id=order.bookId
    WHERE

-}

-- firstOrderSortedBy ::
--   (Database.Beam.Backend.SQL.BeamSqlBackend be,
--     SqlEq
--       (QGenExpr QValueContext be (QNested (QNested s)))
--       (Database.Beam.Schema.Tables.Columnar f Bool),
--     SqlValable (Database.Beam.Schema.Tables.Columnar f Bool),
--     ProjectibleWithPredicate
--       AnyType
--       be
--       (WithExprContext (BeamSqlBackendExpressionSyntax' be))
--       (WithRewrittenThread
--         (QNested (QNested s)) (QNested s) (Order.OrderT f)),
--     ProjectibleWithPredicate
--       AnyType
--       be
--       (WithExprContext (BeamSqlBackendExpressionSyntax' be))
--       (Order.OrderT f),
--     SqlOrderable be ordering,
--     ThreadRewritable
--       (QNested s)
--       (WithRewrittenThread
--         (QNested (QNested s)) (QNested s) (Order.OrderT f)),
--     ThreadRewritable (QNested (QNested s)) (Order.OrderT f))
--     => Q be db (QNested (QNested s)) (Order.OrderT f)
--     -> HaskellLiteralForQExpr
--           (Database.Beam.Schema.Tables.Columnar f Bool)
--     -> (Database.Beam.Schema.Tables.Columnar f Double -> ordering)
--     -> Q be
--           db
--           s
--           (WithRewrittenThread
--             (QNested s)
--             s
--             (WithRewrittenThread
--                 (QNested (QNested s)) (QNested s) (Order.OrderT f)))
firstOrderSortedBy orders isBuyOrder priceOrdering = do
    limit_ 1 $
      orderBy_ (priceOrdering . Order.orderPrice) $
      filterIsBuyOrder $
      orders
  where
    filterIsBuyOrder = filter_ (\o -> Order.orderIsBuy o ==. val_ isBuyOrder)

allBookOrders ::
    ( Database.Beam.Backend.SQL.BeamSqlBackend be
    , HasSqlEqualityCheck be Book.Word32)
    => Q be DB.OrderBookDb s
          ( Book.BookT (QExpr be s)
          , Order.OrderT (QExpr be s)
          )
allBookOrders = do
    book <- all_ (DB.books DB.orderBookDb)
    orders <- DB.bookOrders book
    return (book, orders)
