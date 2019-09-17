{-# LANGUAGE OverloadedStrings #-}
module CryptoDepth.OrderBook.Db.Run.CreateTable
( dbCreate
, createTables
, dropTables
)
where

import qualified CryptoDepth.OrderBook.Db.Run.Migration     as Run
import qualified CryptoDepth.OrderBook.Db.CreateTable       as CT

import           Database.Beam.Migrate.Simple
import qualified Database.Beam.Postgres                     as Pg


dbCreate
    :: MigrationSteps Pg.Postgres () (CheckedDatabaseSettings Pg.Postgres CT.OrderBookDb)
dbCreate =
    migrationStep "Initial migration" CT.createTables

createTables
    :: Pg.Connection
    -> IO (CheckedDatabaseSettings Pg.Postgres CT.OrderBookDb)
createTables conn =
    Run.runMigration conn dbCreate

dropTables
    :: Pg.Connection
    -> CheckedDatabaseSettings Pg.Postgres CT.OrderBookDb
    -> IO ()
dropTables conn checkedDb =
    Run.runMigration conn (dbDelete checkedDb)
  where
    dbDelete
        :: CheckedDatabaseSettings Pg.Postgres CT.OrderBookDb
        -> MigrationSteps Pg.Postgres () ()
    dbDelete checkedDb =
        migrationStep "Delete initial" (\_ -> CT.dropTables checkedDb)
