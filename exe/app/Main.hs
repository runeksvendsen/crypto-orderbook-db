module Main
( main
) where

import           Prelude
import qualified Options
import           Protolude.Conv                         (toS)
import qualified OrderBook.Types                        as OB
import qualified CryptoDepth.OrderBook.Db.Run.Assert    as Assert

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

import           Data.Proxy                             (Proxy(..))
import           Control.Error                          (lefts, rights)
import           Control.Monad                          (forM_)


main :: IO ()
main = Options.withArgs $ \args -> do
    conn <- Postgres.connectPostgreSQL (Options.dbConnString args)
    Assert.assertSchema conn
    books <- fetchBooks (Options.fetchMaxRetries args)
    -- TODO: write books to DB
    undefined

fetchBooks :: Word -> IO [ABook]
fetchBooks maxRetries = do
    man <- HTTP.newManager HTTPS.tlsManagerSettings
    booksE <-
        throwErrM $
        withLogging $
        AppM.runAppM man maxRetries $
        allBooks
    -- Log errors
    forM_ (lefts booksE) logFetchError
    -- Write JSON books
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
    logLevel = Log.LevelDebug

-- | Fetch books, in parallel, from all venues.
--
-- Error handling: for any given venue, return either *all* available
--  order books or an error.
allBooks
    :: AppM.AppM IO [Either AppM.Error [ABook]]
allBooks = do
    Par.forM Venues.allVenues $ \(CryptoVenues.AnyVenue p) ->
         AppM.evalAppM (map toABook <$> venueBooks p)

-- | Fetch all books for a given venue
venueBooks
    :: CryptoVenues.MarketBook venue
    => Proxy venue
    -> AppM.AppM IO [OB.AnyBook venue]
venueBooks _ = do
    allMarkets <- EnumMarkets.marketList
    mapM fetchMarketBook allMarkets
