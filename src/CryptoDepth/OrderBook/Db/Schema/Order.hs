-- {-# LANGUAGE UndecidableInstances #-}
module CryptoDepth.OrderBook.Db.Schema.Order
( OrderT(..)
, Order
, OrderId
, PrimaryKey(type OrderId)
)
where

import CryptoDepth.OrderBook.Db.Internal.Prelude

import CryptoDepth.OrderBook.Db.Schema.Book     (BookT)
import Database.Beam                            (C, Identity, PrimaryKey)
import qualified Database.Beam                  as Beam


-- | Order.
--   NB: Primary key relies on no order book containing
--    two same-priced orders of the same kind (buy/sell).
data OrderT f
    = Order
    { orderBook    :: PrimaryKey BookT f
      -- | Is it a buy order? (if not, it's a sell order)
    , orderIsBuy   :: C f Bool
    , orderQty     :: C f Double
    , orderPrice   :: C f Double
    } deriving Generic

type Order = OrderT Identity
type OrderId = PrimaryKey OrderT Identity

deriving instance Show Order
deriving instance Eq Order

instance Beam.Beamable OrderT

instance Beam.Table OrderT where
    -- NB: Assumes no same-priced orders of same
    --  kind (sell/buy) in the same order book
    data PrimaryKey OrderT f = OrderId
        (PrimaryKey BookT f)
        (C f Bool)
        (C f Double)
            deriving Generic
    primaryKey Order{..} = OrderId orderBook orderIsBuy orderPrice

instance Beam.Beamable (PrimaryKey OrderT)
