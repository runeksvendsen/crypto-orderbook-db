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
import qualified Data.Time.Clock        as C


-- | Indicates whether to pause after running the action,
--    or immediately run it again.
data PauseAction
    = Pause
    | NoPause

-- | Do something forever while pausing a random interval between.
foreverWithPauseRange
    :: forall minPause maxPause.
       ( TU.TimeUnit minPause
       , TU.TimeUnit maxPause
       , Show minPause
       )
    => minPause
    -> maxPause
    -> IO PauseAction
    -> IO Void
foreverWithPauseRange minPause maxPause action =
    forever $ do
        pauseAction <- action
        case pauseAction of
            NoPause ->
                logInfoS "RUNNER" "Not pausing (failure detected)"
            Pause -> do
                pauseDuration <- TU.fromMicroseconds <$> Random.randomRIO
                    (TU.toMicroseconds minPause, TU.toMicroseconds maxPause)
                durationEnd <- durationEndTime pauseDuration
                logInfoS "RUNNER" $ "Pausing until " ++ show durationEnd
                threadDelay (fromIntegral $ TU.toMicroseconds pauseDuration)
                logInfoS "RUNNER" "Pause over"
  where
    logInfoS = Log.loggingLogger Log.LevelInfo
    addDuration :: C.UTCTime -> TU.Picosecond -> C.UTCTime
    addDuration utcTime duration = C.addUTCTime
        (fromRational $ (realToFrac (TU.toMicroseconds duration) :: Rational) / 1e6)
        utcTime
    durationEndTime duration = do
        now <- C.getCurrentTime
        return (addDuration now duration)

-- | Run an IO action, and catch & log a thrown exception
--    as an error.
--   Returns 'NoPause' if an exception occurs, otherwise 'Pause'.
logSwallowExceptions
    :: IO ()
    -> IO PauseAction
logSwallowExceptions action =
    actionSuccess `catch` \someException -> do
        logErrorS "MAIN" (show (someException :: SomeException))
        return NoPause
  where
    logErrorS = Log.loggingLogger Log.LevelError
    actionSuccess = do
        action
        return Pause
