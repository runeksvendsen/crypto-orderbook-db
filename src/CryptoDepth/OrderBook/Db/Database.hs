module CryptoDepth.OrderBook.Db.Database
( OrderBookDb(..)
, orderBookDb
, bookOrders
, runBooks
)
where

import CryptoDepth.OrderBook.Db.Internal.Prelude
import qualified CryptoDepth.OrderBook.Db.Schema.Run    as Run
import qualified CryptoDepth.OrderBook.Db.Schema.Book   as Book
import qualified CryptoDepth.OrderBook.Db.Schema.Order  as Order
import qualified Database.Beam                          as Beam


data OrderBookDb f = OrderBookDb
    { runs     :: f (Beam.TableEntity Run.RunT)
    , books    :: f (Beam.TableEntity Book.BookT)
    , orders   :: f (Beam.TableEntity Order.OrderT)
    } deriving Generic

instance Beam.Database be OrderBookDb

orderBookDb :: Beam.DatabaseSettings be OrderBookDb
orderBookDb = Beam.defaultDbSettings

bookOrders
    :: Beam.HasSqlEqualityCheck be Book.Int32
    => Beam.OneToMany be OrderBookDb s Book.BookT Order.OrderT
bookOrders = Beam.oneToMany_ (orders orderBookDb) Order.orderBook

runBooks
    :: Beam.HasSqlEqualityCheck be Int32
    => Beam.OneToMany be OrderBookDb s Run.RunT Book.BookT
runBooks = Beam.oneToMany_ (books orderBookDb) Book.bookRun
