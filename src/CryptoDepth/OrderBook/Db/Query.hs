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
import qualified Data.Vector as Vec
import qualified Database.Beam.Postgres as Pg


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
    return ( ( orderVector book True
             , orderVector book False
             )
           , ( Book.bookVenue book
             , ( Book.bookBase book
               , Book.bookQuote book
               )
             )
           )
  where
    orders book isBuy = filter_ (\o -> Order.orderIsBuy o ==. val_ isBuy) (DB.bookOrders book)
    orderVector book isBuy = Pg.arrayOf_ $
          (\o -> Pg.array_ [Order.orderPrice o, Order.orderQty o]) <$> orders book isBuy

runBooks runId = do
    orders <- runSelectReturningList $ select (runOrders runId)
    return $ map fromGroupedOrders orders
  where
    fromOrder orderVec = (orderVec Vec.! 0, orderVec Vec.! 1)
    fromGroupedOrders :: ((Vec.Vector (Vec.Vector Double), Vec.Vector (Vec.Vector Double)), (Text, (Text, Text))) -> OB
    fromGroupedOrders ((buy, sell), (venue, (base, quote))) =
        OB venue base quote (Vec.map fromOrder buy) (Vec.map fromOrder sell)

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
