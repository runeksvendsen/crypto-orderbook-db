{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE StrictData #-}
module CryptoDepth.OrderBook.Db.Query
( buySellPriceRange
, volumeBaseQuote
, DB.bookOrders
, runBooks
, OB(..)
, Order
)
where

import CryptoDepth.OrderBook.Db.Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Schema.Order  as Order
import qualified CryptoDepth.OrderBook.Db.Schema.Book as Book
import qualified CryptoDepth.OrderBook.Db.Database               as DB

import Database.Beam.Query
import Data.List (partition, sortOn, groupBy)
import qualified Data.Vector as Vec


-- | Total volume of sell orders and buy orders, respectively,
--    measured in both base currency and quote currency.
--   Output: (base_volume, quote_volume)
volumeBaseQuote bookId =
    aggregate_ groupAndSum (DB.bookOrders bookId)
  where
    groupAndSum orders =
        ( ( group_ $ Order.orderBook orders
          , group_ $ Order.orderIsBuy orders
          )
        , ( sum_ $ Order.orderQty orders
          , sum_ $ Order.orderQty orders **. Order.orderPrice orders
          )
        )

-- | Price range of buy orders and sell orders, respectively.
--   Output:
--      ( (highest_buy_order_price, lowest_buy_order_price)
--      , (highest_sell_order_price, lowest_sell_order_price)
--      )
--  Can be used to calculate:
--      a) Spread (lowest_sell_order_price - highest_buy_order_price)
--      b) Sell/buy order price range:
--          percentage difference between best-priced and worst-priced order
buySellPriceRange bookId = do
    highestPricedBid <- firstOrderSortedBy bookId True desc_
    lowestPricedBid <- firstOrderSortedBy bookId True asc_
    highestPricedAsk <- firstOrderSortedBy bookId False desc_
    lowestPricedAsk <- firstOrderSortedBy bookId False asc_
    return
        ( (price highestPricedBid, price lowestPricedBid)
        , (price highestPricedAsk, price lowestPricedAsk)
        )
  where
    price = Order.orderPrice

firstOrderSortedBy bookId isBuyOrder ordering =
    limit_ 1 $
    orderBy_ (ordering . Order.orderPrice) $
    filterIsBuyOrder $
    DB.bookOrders bookId
  where
    filterIsBuyOrder = filter_ (\o -> Order.orderIsBuy o ==. val_ isBuyOrder)

runOrders runId = do
    book <- all_ (DB.books DB.orderBookDb)
    guard_ $ Book.bookRun book ==. val_ runId
    order <- DB.bookOrders book
    return (order, (Book.bookVenue book, (Book.bookBase book, Book.bookQuote book)))

runBooks runId = do
    orders <- runSelectReturningList $ select (runOrders runId)
    let groupedOrders = map (\lst -> (map fst lst, snd $ head lst) ) $ groupOn snd orders
    return $ map fromGroupedOrders groupedOrders
  where
    groupOn f = groupBy (\a1 a2 -> f a1 == f a2) . sortOn f
    fromOrder order = (Order.orderPrice order, Order.orderQty order)
    fromGroupedOrders :: ([Order.Order], (Text, (Text, Text))) -> OB
    fromGroupedOrders (orderList, (venue, (base, quote))) =
        let (buy, sell) = partition Order.orderIsBuy orderList
        in OB venue base quote (Vec.fromList $ map fromOrder buy) (Vec.fromList $ map fromOrder sell)

-- | (price, qty)
type Order = (Double, Double)

-- |
data OB = OB
    { obVenue :: Text
    , obBase :: Text
    , obQuote :: Text
    , obBuyOrders :: Vec.Vector Order
    , obSellOrders :: Vec.Vector Order
    }
