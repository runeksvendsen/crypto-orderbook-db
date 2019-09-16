{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CryptoDepth.OrderBook.Db.Query
( buySellPriceRange
, volumeBaseQuote
, bookOrders
)
where

import qualified CryptoDepth.OrderBook.Db.Schema.Order  as Order
import qualified CryptoDepth.OrderBook.Db.Database               as DB

import Database.Beam.Query


-- | Total volume of sell orders and buy orders, respectively,
--    measured in both base currency and quote currency.
--   Output: (base_volume, quote_volume)
volumeBaseQuote bookId =
    aggregate_ groupAndSum (bookOrders bookId)
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
    bookOrders bookId
  where
    filterIsBuyOrder = filter_ (\o -> Order.orderIsBuy o ==. val_ isBuyOrder)

bookOrders bookId = do
    order <- all_ (DB.orders DB.orderBookDb)
    guard_ (Order.orderBook order `references_` bookId)
    return order
