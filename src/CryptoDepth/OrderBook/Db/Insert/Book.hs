module CryptoDepth.OrderBook.Db.Insert.Book
( storeBook
, BookId
)
where

-- import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.OrderBook.Db.Schema.Book         as Book
import qualified CryptoDepth.OrderBook.Db           as DB

import qualified Database.Beam                      as Beam
import Database.Beam.Postgres                       (Pg)
import Database.Beam.Backend.SQL.BeamExtensions     (runInsertReturningList)


storeBook = undefined

-- | Insert orderbook, returning the assigned ID.
-- storeBook :: Book.LocalTime -> Pg Book.BookId
-- storeBook time = fmap (Beam.pk . getSingleResult) $
--     runInsertReturningList $
--         Beam.insert (DB.books DB.orderBookDb) $
--             Beam.insertExpressions [ book ]
--   where
--     book = Book
--         { bookId       = Beam.default_
--         , bookTime     = bookTime
--         , bookVenue    = bookVenue
--         , bookBase     = bookBase
--         , bookQuote    = bookQuote
--         }
