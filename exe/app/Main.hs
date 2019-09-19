{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

import           Prelude
import qualified Options
import qualified Runner
import           Protolude.Conv                         (toS)
import qualified OrderBook.Types                        as OB
import qualified CryptoDepth.OrderBook.Db.Insert        as Insert
import           CryptoDepth.OrderBook.Db.Insert        (SomeOrderBook)
import qualified CryptoDepth.OrderBook.Db.Monad         as Db

-- CryptoVenues
import qualified CryptoVenues
import           CryptoVenues.Types.ABook
import qualified CryptoVenues.Fetch.EnumMarkets         as EnumMarkets
import qualified CryptoVenues.Venues                    as Venues
import qualified CryptoVenues.Types.AppM                as AppM
import qualified CryptoVenues.Types.Error               as AppMErr
import           CryptoVenues.Fetch.MarketBook          (fetchMarketBook)

import qualified Database.Beam.Postgres                 as Postgres
import qualified Data.Text                              as T
import qualified Control.Monad.Parallel                 as Par
import qualified Network.HTTP.Client.TLS                as HTTPS
import qualified Network.HTTP.Client                    as HTTP
import qualified Control.Logging                        as Log
import qualified Data.Time.Clock                        as Clock

import           Data.Proxy                             (Proxy(..))
import           Control.Error                          (lefts, rights)
import           Control.Monad                          (forM, forM_)
import           Control.Monad.IO.Class                 (liftIO)


main :: IO Runner.Void
main = Options.withArgs $ \args ->
    withLogging $
        -- Keep doing the below with a random pause in-between (in specified range)
        Runner.foreverWithPauseRange (10 :: Runner.Minute) (30 :: Runner.Minute) $
            -- Catch all exceptions and log them as an error
            Runner.logSwallowExceptions $
                connectFetchStore args

-- | Open database connection,
--   fetch orderbooks,
--   write orderbooks to database,
--   close database connection.
connectFetchStore :: Options.Options -> IO ()
connectFetchStore args = do
    logInfoS "DB" ("Connecting to " ++ show dbConnString)
    conn <- Postgres.connectPostgreSQL dbConnString
    bookFetchRun <- fetchRun (Options.fetchMaxRetries args)
    -- Don't store empty run
    if length (timeBookList bookFetchRun) > 0
        then storeBooks conn bookFetchRun
        else logInfoS "MAIN" "Not inserting empty run"
    Postgres.close conn
  where
    dbConnString = Options.dbConnString args

storeBooks :: Postgres.Connection -> BookRun -> IO ()
storeBooks conn BookRun{..} = do
    (runId, bookIdList) <- Db.runDb conn $
        Insert.storeRun runStartTime runEndTime timeBookList
    logInfoS (toS $ show runId)
             ("Inserted " ++ show (length bookIdList) ++ " books")

logInfoS :: T.Text -> String -> IO ()
logInfoS = Log.loggingLogger Log.LevelInfo

fetchRun :: Word -> IO BookRun
fetchRun maxRetries = do
    runStartTime <- Clock.getCurrentTime
    timeBookList <- fetchBooks maxRetries
    runEndTime <- Clock.getCurrentTime
    return $ BookRun runStartTime timeBookList runEndTime

data BookRun = BookRun
    { runStartTime  :: Clock.UTCTime
    , timeBookList  :: [(Clock.UTCTime, SomeOrderBook)]
    , runEndTime    :: Clock.UTCTime
    }

-- TODO: error handling when running "EnumMarkets.marketList"
fetchBooks :: Word -> IO [(Clock.UTCTime, SomeOrderBook)]
fetchBooks maxRetries = do
    man <- HTTP.newManager HTTPS.tlsManagerSettings
    booksE <- throwErrM $
        AppM.runAppM man maxRetries $ allBooks
    -- Log errors
    forM_ (lefts booksE) logFetchError
    return . concat . rights $ booksE
  where
    logErrorS :: T.Text -> T.Text -> IO ()
    logErrorS = Log.loggingLogger Log.LevelError
    throwErrM ioA =
        ioA >>= either (error . show) return
    logFetchError (AppMErr.Error ctx err) =
        logErrorS (toS $ show ctx) (toS $ show err)

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T.%3q"
    ioa
  where
    -- TODO: don't hardcode
    logLevel :: Log.LogLevel
    logLevel = Log.LevelInfo

-- | Fetch books, in parallel, from all venues.
--
-- Error handling: for any given venue, return either *all* available
--  order books or an error.
allBooks
    :: AppM.AppM IO [Either AppM.Error [(Clock.UTCTime, SomeOrderBook)]]
allBooks = do
    Par.forM Venues.allVenues $ \(CryptoVenues.AnyVenue p) ->
         AppM.evalAppM (map (fmap $ toSomeOrderBook . toABook) <$> venueBooks p)
  where
    toSomeOrderBook (ABook ob) = Insert.SomeOrderBook ob

-- | Fetch all books for a given venue
venueBooks
    :: CryptoVenues.MarketBook venue
    => Proxy venue
    -> AppM.AppM IO [(Clock.UTCTime, OB.AnyBook venue)]
venueBooks _ = do
    allMarkets <- EnumMarkets.marketList
    forM allMarkets $ \market -> do
        book <- fetchMarketBook market
        time <- liftIO Clock.getCurrentTime
        return (time, book)
