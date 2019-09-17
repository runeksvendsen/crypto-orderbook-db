module Main
( main
) where

import qualified CryptoDepth.OrderBook.Db.Run.CreateTable   as CT

import qualified Options
import qualified Options.Applicative                        as Opt
import qualified Data.ByteString                            as BS
import qualified Database.Beam.Postgres                     as Postgres
import qualified Database.Beam.Migrate.Types                as Migrate


main :: IO ()
main = do
    connString <- Opt.execParser opts
    conn <- Postgres.connectPostgreSQL connString
    checkedDb <- CT.createTables conn
    putStrLn "Created database:"
    print $ Migrate.collectChecks checkedDb

opts :: Opt.ParserInfo BS.ByteString
opts =
    Opt.info Options.connString' $
       Opt.fullDesc
    <> Opt.header "Write database tables to database"
