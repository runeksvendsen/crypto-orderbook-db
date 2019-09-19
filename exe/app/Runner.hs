{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Runner
( foreverWithPauseRange
, logSwallowExceptions
-- * Re-exports
, Void
-- * Time units
, TU.Hour
, TU.Minute
, TU.Second
)
where


import           Data.Void              (Void)
import           Control.Monad          (forever)
import           Control.Concurrent     (threadDelay)
import           Control.Exception      (catch, SomeException)

import qualified Data.Time.Units        as TU
import qualified System.Random          as Random
import qualified Control.Logging        as Log


-- | Do something forever while pausing a random interval between.
foreverWithPauseRange
    :: forall minPause maxPause a.
       ( TU.TimeUnit minPause
       , TU.TimeUnit maxPause
       , Show minPause
       )
    => minPause
    -> maxPause
    -> IO a
    -> IO Void
foreverWithPauseRange minPause maxPause action =
    forever $ do
        _ <- action
        pauseMicroseconds <- Random.randomRIO
            (TU.toMicroseconds minPause, TU.toMicroseconds maxPause)
        logInfoS "RUNNER" $ "Pausing for " ++ show (TU.fromMicroseconds pauseMicroseconds :: minPause)
        threadDelay (fromIntegral pauseMicroseconds)
  where
    logInfoS = Log.loggingLogger Log.LevelInfo

-- | Run an IO action, and catch & log a thrown exception
--    as an error.
logSwallowExceptions
    :: IO ()
    -> IO ()
logSwallowExceptions action =
    action `catch` \someException ->
        logErrorS "MAIN" (show (someException :: SomeException))
  where
    logErrorS = Log.loggingLogger Log.LevelError
