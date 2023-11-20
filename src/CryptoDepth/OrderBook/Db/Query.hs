{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE StrictData #-}
module CryptoDepth.OrderBook.Db.Query
( buySellPriceRange
, volumeBaseQuote
, DB.bookOrders
, runBooks
, runBook
, newestRun
, OB(..)
, Order
)
where

import CryptoDepth.OrderBook.Db.Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Schema.Order  as Order
import qualified CryptoDepth.OrderBook.Db.Schema.Book as Book
import qualified CryptoDepth.OrderBook.Db.Database               as DB
import qualified CryptoDepth.OrderBook.Db.Schema.Run as Run

import Database.Beam.Query
import qualified Data.Vector as Vec
import qualified Database.Beam.Postgres as Pg

-- | Return the most recent run.
--
--   If there are no runs at all then 'Nothing' is returned.
newestRun
  :: Pg.Pg (Maybe Run.Run)
newestRun =
    runSelectReturningOne $ select newestRunQ
  where
    newestRunQ
        :: Q Pg.Postgres DB.OrderBookDb s (Run.RunT (QExpr Pg.Postgres s))
    newestRunQ = do
        maxRunId <- aggregate_
          max_
          (Run.runId <$> all_ (DB.runs DB.orderBookDb))
        filter_
          (\run -> just_ (Run.runId run) ==. maxRunId)
          (all_ $ DB.runs DB.orderBookDb)

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

type RunOrdersExpr ctx1 ctx2 s =
  ( (QGenExpr ctx1 Pg.Postgres s (Vec.Vector (Vec.Vector Double)),
        QGenExpr ctx2 Pg.Postgres s (Vec.Vector (Vec.Vector Double)))
  , (QExpr Pg.Postgres s Text,
        (QExpr Pg.Postgres s Text,
          QExpr Pg.Postgres s Text))
  )

runOrders
  :: Run.RunId
  -> Q Pg.Postgres
      DB.OrderBookDb
      s
      (RunOrdersExpr ctx1 ctx2 s)
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

runBooks :: Run.RunId -> Pg.Pg [OB]
runBooks = runBooks' (const $ pure ())

runBooks'
  :: (RunOrdersExpr ctx1 ctx2 QBaseScope -> Q Pg.Postgres DB.OrderBookDb QBaseScope ())
  -> Run.RunId -> Pg.Pg [OB]
runBooks' guard runId = do
    orders <- runSelectReturningList $ select $ do
      book <- runOrders runId
      guard book
      pure book
    return $ map fromGroupedOrders orders
  where
    fromOrder orderVec = (orderVec Vec.! 0, orderVec Vec.! 1)
    fromGroupedOrders :: ((Vec.Vector (Vec.Vector Double), Vec.Vector (Vec.Vector Double)), (Text, (Text, Text))) -> OB
    fromGroupedOrders ((buy, sell), (venue, (base, quote))) =
        OB venue base quote (Vec.map fromOrder buy) (Vec.map fromOrder sell)

-- | Get one orderbook for a particular run and venue
--   for two known currencies without knowing which currency is 'base' and which is 'quote'.
--
--   Note that we expect our orderbook database to contain exactly one orderbook for a given
--   (runId, venue, currencyPair) -- ie. we expect our DB to _not_ contain an orderbook
--   for both (base=X, quote=Y) AND (base=Y, quote=X).
--
--   This enables us to retrieve an orderbook from a path through a graph, where we no longer
--   know which is the base currency and which is the quote currency.
runBook
  :: Text -- ^ Venue
  -> Text -- ^ currency1: either base or quote
  -> Text -- ^ currency2: @if currency1 is base then quote else base@
  -> Run.RunId
  -> Pg.Pg [OB]
runBook venue bookCurrency1 bookCurrency2 =
    runBooks' guard
  where
    guard (_, (venue', (base, quote))) = guard_ $
          venue' ==. val_ venue &&.
      (   (base ==. val_ bookCurrency1 &&. quote ==. val_ bookCurrency2)
      ||. (quote ==. val_ bookCurrency1 &&. base ==. val_ bookCurrency2))

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
