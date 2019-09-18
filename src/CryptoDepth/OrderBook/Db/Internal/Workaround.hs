{-# LANGUAGE OverloadedStrings #-}
module CryptoDepth.OrderBook.Db.Internal.Workaround
(
)
where

-- import           CryptoDepth.OrderBook.Db.Internal.Prelude
-- import qualified CryptoDepth.OrderBook.Db.Run.CreateTable   as CT

-- import           Data.List                                  ((\\))

import qualified Data.Type.Equality                         as TEq
import           Data.Proxy                                 (Proxy(Proxy))
import qualified Database.Beam.Migrate.Simple               as Migrate
import qualified Database.Beam.Migrate.SQL.Tables           as MT
import qualified Database.Beam.Migrate.Checks               as MC
import qualified Database.Beam.Postgres                     as Pg
-- TMP
import qualified Database.Beam.Migrate.SQL as MSQL
-- TMP


-- lol :: Migrate.SomeDatabasePredicate -> Bool
-- lol (Migrate.SomeDatabasePredicate p) =
--     case TEq.testEquality (Proxy :: Proxy (MC.TableColumnHasConstraint Pg.Postgres)) (lol p) of
--         Nothing   -> False
--         Just TEq.Refl -> case p of
--             (MC.TableColumnHasConstraint _ _ cds) -> undefined


--         -- ""
--         -- (MSQL.constraintDefinitionSyntax Nothing MSQL.uniqueColumnConstraintSyntax Nothing))
--   where
--     lol :: p -> Proxy p
--     lol _ = Proxy

uniqueConstraint :: MC.TableColumnHasConstraint Pg.Postgres
uniqueConstraint =
    MC.TableColumnHasConstraint
        undefined
        ""
        (MSQL.constraintDefinitionSyntax Nothing MSQL.uniqueColumnConstraintSyntax Nothing)
