module CryptoDepth.OrderBook.Db.Schema.Run
( RunT(..)
, Run
, RunId
, PrimaryKey(type RunId)
  -- * Re-exports
, UTCTime
, Word32
)
where

import CryptoDepth.OrderBook.Db.Internal.Prelude

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)
import Data.Time.Clock                      (UTCTime)
import Database.Beam.Backend.SQL.Types      (SqlSerial)
import Data.Word                            (Word32)


-- | A time span within which a collection of order books
--    were fetched.
data RunT f
    = Run
    { runId         :: C f (SqlSerial Word32)
    , runTimeStart  :: C f UTCTime
    , runTimeEnd    :: C f UTCTime
    } deriving Generic

type Run = RunT Identity
type RunId = PrimaryKey RunT Identity

deriving instance Show Run
deriving instance Eq Run
deriving instance Show RunId
deriving instance Eq RunId

instance Beam.Beamable RunT

instance Beam.Table RunT where
    data PrimaryKey RunT f = RunId
        (C f (SqlSerial Word32))
            deriving Generic
    primaryKey = RunId . runId

instance Beam.Beamable (PrimaryKey RunT)
