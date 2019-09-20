{-# LANGUAGE ExistentialQuantification #-}
module CryptoDepth.OrderBook.Db.Insert
( M.Db
, storeRun
, Book.BookId
, Run.RunId
, SomeOrderBook(..)
)
where

import CryptoDepth.OrderBook.Db.Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Util              as Util
import qualified CryptoDepth.OrderBook.Db.Schema.Run        as Run
import qualified CryptoDepth.OrderBook.Db.Schema.Book       as Book
import qualified CryptoDepth.OrderBook.Db.Schema.Order      as Order
import qualified CryptoDepth.OrderBook.Db.Database          as DB
import qualified CryptoDepth.OrderBook.Db.Monad             as M

import qualified OrderBook.Types                            as OB
import qualified Database.Beam                              as Beam
import qualified Data.Vector                                as Vec
import Database.Beam.Backend.SQL.BeamExtensions             (runInsertReturningList)
import Database.Beam.Postgres                               (Pg)
import qualified Money


data SomeOrderBook = forall venue base quote.
   ( KnownSymbol venue
   , KnownSymbol base
   , KnownSymbol quote
   ) => SomeOrderBook (OB.OrderBook venue base quote)

-- | Store an entire run as a transaction
storeRun
    :: Book.UTCTime                         -- ^ Run start time
    -> Book.UTCTime                         -- ^ Run end time
    -> [(Book.UTCTime, SomeOrderBook)]      -- ^ Books & fetch times
    -> M.Db (Run.RunId, [Book.BookId])
storeRun startTime endTime books = (M.runTx <=< M.asTx) $ do
    runId <- insertRunReturningId $ Beam.insertExpressions
        [ Run.Run
            { runId         = Beam.default_
            , runTimeStart  = Beam.val_ startTime
            , runTimeEnd    = Beam.val_ endTime
            }
        ]
    bookIdList <- forM books $ \(time, SomeOrderBook book) -> do
        -- NB: merging same-priced orders is important since the order primary key depends
        --  on two same-priced orders not existing in the same order book
        storeBookPg runId time (Util.mergeSamePricedOrders book)
    return (runId, bookIdList)
  where
    insertRunReturningId =
        fmap (Beam.pk . getSingleResult "storeRun")
        . runInsertReturningList
        . Beam.insert (DB.runs DB.orderBookDb)

storeBookPg
    :: forall venue base quote.
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
    => Run.RunId
    -> Book.UTCTime
    -> OB.OrderBook venue base quote
    -> Pg Book.BookId
storeBookPg runId time book = do
    bookId <- insertBookReturningId $ Beam.insertExpressions
        [ Book.Book
            { bookId       = Beam.default_
            , bookRun      = Beam.val_ runId
            , bookTime     = Beam.val_ time
            , bookVenue    = Beam.val_ $ toS $ symbolVal (Proxy :: Proxy venue)
            , bookBase     = Beam.val_ $ toS $ symbolVal (Proxy :: Proxy base)
            , bookQuote    = Beam.val_ $ toS $ symbolVal (Proxy :: Proxy quote)
            }
        ]
    insertOrders $ map (toOrder True bookId) (Vec.toList . OB.buySide . OB.obBids $ book)
    insertOrders $ map (toOrder False bookId) (Vec.toList . OB.sellSide . OB.obAsks $ book)
    return bookId
  where
    insertBookReturningId =
        fmap (Beam.pk . getSingleResult "storeBook")
        . runInsertReturningList
        . Beam.insert (DB.books DB.orderBookDb)
    insertOrders =
        Beam.runInsert
        . Beam.insert (DB.orders DB.orderBookDb)
        . Beam.insertValues
    toOrder :: Bool -> Book.BookId -> OB.Order base quote -> Order.Order
    toOrder isBuy bookId order =
        Order.Order
            { orderBook    = bookId
            , orderIsBuy   = isBuy
            , orderQty     = realToFrac . toRational . OB.oQuantity $ order
            , orderPrice   = realToFrac . Money.exchangeRateToRational . OB.oPrice $ order
            }

getSingleResult :: Show a => String -> [a] -> a
getSingleResult identifier lst =
    fromMaybe (error $ identifier ++ ": no (single) result" ++ show lst )
        $ headMay lst
