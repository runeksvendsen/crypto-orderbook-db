module CryptoDepth.OrderBook.Db.Schema.Book
( BookT(..)
, Book
, BookId
, PrimaryKey(type BookId)
  -- * Re-exports
, LocalTime
, Word32
)
where

import CryptoDepth.OrderBook.Db.Internal.Prelude

import qualified Database.Beam              as Beam
import           Database.Beam              (C, Identity, PrimaryKey)
import Data.Time.LocalTime                  (LocalTime)
import Database.Beam.Backend.SQL.Types      (SqlSerial)
import Data.Word                            (Word32)


-- | Order book.
--   NB: Primary key for order relies on no order book containing two same-priced orders.
--   Therefore, merge same-priced orders into one before inserting.
data BookT f
    = Book
    { bookId       :: C f (SqlSerial Word32)
    , bookTime     :: C f LocalTime
    , bookVenue    :: C f Text
    , bookBase     :: C f Text
    , bookQuote    :: C f Text
    } deriving Generic

type Book = BookT Identity
type BookId = PrimaryKey BookT Identity

deriving instance Show Book
deriving instance Eq Book
deriving instance Show BookId
deriving instance Eq BookId

instance Beam.Beamable BookT

instance Beam.Table BookT where
    data PrimaryKey BookT f = BookId
        (C f (SqlSerial Word32))
            deriving Generic
    primaryKey = BookId . bookId

instance Beam.Beamable (PrimaryKey BookT)
