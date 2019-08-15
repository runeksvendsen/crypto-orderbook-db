module CryptoDepth.OrderBook.Db
( OrderBookDb(..)
, orderBookDb
, bookOrders
)
where

import CryptoDepth.OrderBook.Db.Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Schema.Book   as Book
import qualified CryptoDepth.OrderBook.Db.Schema.Order  as Order
import qualified Database.Beam                          as Beam


data OrderBookDb f = OrderBookDb
    { _books    :: f (Beam.TableEntity Book.BookT)
    , _orders   :: f (Beam.TableEntity Order.OrderT)
    } deriving Generic

instance Beam.Database be OrderBookDb

orderBookDb :: Beam.DatabaseSettings be OrderBookDb
orderBookDb = Beam.defaultDbSettings

bookOrders
    :: Beam.HasSqlEqualityCheck be Book.Word32
    => Beam.OneToMany be OrderBookDb s Book.BookT Order.OrderT
bookOrders = Beam.oneToMany_ (_orders orderBookDb) Order._orderBook
