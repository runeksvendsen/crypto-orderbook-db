{-# LANGUAGE OverloadedStrings #-}
module CryptoDepth.OrderBook.Db.Run.Assert
( assertSchema
)
where

import           CryptoDepth.OrderBook.Db.Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Run.CreateTable   as CT

import           Data.List                                  ((\\))

import qualified Database.Beam.Migrate.Simple               as Migrate
import qualified Database.Beam.Postgres                     as Pg
import qualified Database.Beam.Postgres.Migrate             as Pg
import qualified Text.Pretty.Simple                         as Pretty


-- | Assert that the database has been migrated to the current schema
assertSchema :: Pg.Connection -> IO ()
assertSchema conn = do
    actualDbState   <- Pg.getDbConstraints conn
    expectedDbState <- Migrate.collectChecks <$> mkCheckedDb CT.dbCreate
    unless (all (`elem` actualDbState) expectedDbState) $ do
        logAssertError $
            "Missing: " <> toS (Pretty.pShow $ expectedDbState \\ actualDbState) <>
            "\nFound: " <> (toS $ Pretty.pShow actualDbState) <>
            "\nExpected: " <> (toS $ Pretty.pShow expectedDbState)
        error "Unsupported DB schema. See details above."
  where
    logAssertError = logErrorS "ASSERT"
    mkCheckedDb migration =
        Migrate.runMigrationSteps 0 Nothing migration (\_ _ step -> pure (Migrate.runMigrationSilenced step))
