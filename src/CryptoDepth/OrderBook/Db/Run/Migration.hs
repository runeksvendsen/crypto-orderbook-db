{-# LANGUAGE OverloadedStrings #-}
module CryptoDepth.OrderBook.Db.Run.Migration
( runMigration
)
where

import           CryptoDepth.OrderBook.Db.Internal.Prelude

import           Control.Exception                      (catch)
import qualified Control.Logging                        as Log

import           Database.Beam.Migrate.Simple
import qualified Database.Beam.Postgres                 as Pg
import qualified Database.Beam.Postgres.Syntax          as Pg
import qualified Database.PostgreSQL.Simple             as PgSimple
import qualified Database.PostgreSQL.Simple.Types       as PgSimple


runMigration
    :: PgSimple.Connection
    -> MigrationSteps Pg.Postgres () a
    -> IO a
runMigration conn migration =
    let executeFunction = (tryExecute conn <=< debugPrintQuery) . newSqlQuery
        tryExecute conn query =
            catch (void $ PgSimple.execute_ conn query)
            (\err -> putStrLn ("ERROR: " ++ show (err :: PgSimple.SqlError)))
    in runMigrationSteps 0 Nothing migration
          (\_ _ -> executeMigration executeFunction)

debugPrintQuery :: PgSimple.Query -> IO PgSimple.Query
debugPrintQuery query =
    Log.debugS "SQL" (toS $ PgSimple.fromQuery query) >> return query

newSqlQuery :: Pg.PgCommandSyntax -> PgSimple.Query
newSqlQuery syntax =
    PgSimple.Query (toS sqlFragment)
  where
    sqlFragment = Pg.pgRenderSyntaxScript . Pg.fromPgCommand $ syntax
