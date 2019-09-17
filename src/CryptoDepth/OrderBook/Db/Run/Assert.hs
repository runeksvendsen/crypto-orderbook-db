{-# LANGUAGE OverloadedStrings #-}
module CryptoDepth.OrderBook.Db.Run.Assert
( assertSchema
)
where

import           CryptoDepth.OrderBook.Db.Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Run.CreateTable   as CT

import           Data.List                                  ((\\))

import           Database.Beam.Migrate.Simple
import           Database.Beam.Migrate.Types                (collectChecks)
import qualified Database.Beam.Postgres                     as Pg
import qualified Database.Beam.Postgres.Migrate             as Pg
import qualified Text.Pretty.Simple                         as Pretty


-- | Assert that the database has been migrated to the current schema
assertSchema :: Pg.Connection -> IO ()
assertSchema conn = do
    actualDbState   <- Pg.getDbConstraints conn
    expectedDbState <- collectChecks <$> mkCheckedDb CT.dbCreate
    unless (all (`elem` actualDbState) expectedDbState) $ do
        putStrLn "Missing:"
        Pretty.pPrint $ expectedDbState \\ actualDbState
        putStrLn "Found:"
        Pretty.pPrint actualDbState
        putStrLn "Expected:"
        Pretty.pPrint expectedDbState
        error "Unsupported DB schema. See details above."
  where
    mkCheckedDb migration =
        runMigrationSteps 0 Nothing migration (\_ _ step -> pure (runMigrationSilenced step))
