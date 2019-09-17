{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoDepth.OrderBook.Db.Monad
( Db
, runDb
, runDbDebug
, liftPg
, asTx
, runTx
)
where

import CryptoDepth.OrderBook.Db.Internal.Prelude
import qualified Database.Beam.Postgres             as Pg

import qualified Control.Monad.Trans.Reader         as R
import qualified Database.PostgreSQL.Transaction    as PgTx
import qualified Database.PostgreSQL.Simple         as Postgres


newtype Db a = Db { unDb :: R.ReaderT DbConfig IO a }
    deriving (Functor, Applicative, Monad)

data DbConfig = DbConfig
    { cfgConnection :: Postgres.Connection
    , cfgLogger     :: String -> IO ()
    }

runDb
    :: Postgres.Connection
    -> Db a
    -> IO a
runDb =
    runDbDebug (\_ -> pure ())

runDbDebug
    :: (String -> IO ())
    -> Postgres.Connection
    -> Db a
    -> IO a
runDbDebug logger conn =
    (`R.runReaderT` cfg) . unDb
  where
    cfg = DbConfig conn logger

liftPg
    :: Pg.Pg a
    -> Db a
liftPg pg = Db $ do
    conn <- R.asks cfgConnection
    logger <- R.asks cfgLogger
    liftIO $ Pg.runBeamPostgresDebug logger conn pg

runTx
    :: PgTx.PGTransaction a
    -> Db a
runTx tx = Db $ do
    conn <- R.asks cfgConnection
    liftIO $ PgTx.runPGTransactionT tx conn

asIO
    :: Db a
    -> Db (IO a)
asIO dba = Db $ do
    cfg <- R.ask
    return $ runDbDebug (cfgLogger cfg) (cfgConnection cfg) dba

asTx
    :: Pg.Pg a
    -> Db (PgTx.PGTransaction a)
asTx pga = do
    pgIO <- asIO $ liftPg pga
    return $ liftIO pgIO
