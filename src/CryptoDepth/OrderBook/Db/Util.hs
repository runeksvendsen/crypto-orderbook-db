module CryptoDepth.OrderBook.Db.Util
( mergeSamePricedOrders
, sortByOccurrenceCount
)
where

import qualified OrderBook.Types                    as OB
import           GHC.Exts                           (IsList(..))
import           Data.List                          (groupBy, sortOn)
import           Data.Ord                           (Down(Down))
import qualified Data.Map.Strict                    as Map


-- | Merge same-priced orders
mergeSamePricedOrders
    :: OB.OrderBook venue base quote
    -- ^ Order book
    -> OB.OrderBook venue base quote
    -- ^ All same-priced buy/sell orders have been merged into one buy/sell order
mergeSamePricedOrders book =
    OB.OrderBook
        { obBids = OB.BuySide . asList (mergeSamePrice . sortOnDesc OB.oPrice) . OB.buySide . OB.obBids $ book
        , obAsks = OB.SellSide . asList (mergeSamePrice . sortOn OB.oPrice) . OB.sellSide . OB.obAsks $ book
        }
  where
    asList f = fromList . f . toList

-- | merge adjacent orders with same price
mergeSamePrice
    :: [OB.Order base quote]
    -- ^ List of orders sorted by price
    -> [OB.Order base quote]
    -- ^ List of orders where same-priced orders have been merged
mergeSamePrice =
    map mergeOrders . groupBy (\o1 o2 -> OB.oPrice o1 == OB.oPrice o2)
  where
    mergeOrders orderList =
        let order1 = head orderList -- safe since 'groupBy' produces non-empty lists
        in order1 { OB.oQuantity = sum $ map OB.oQuantity orderList }

-- | Sort each sublist in a list so that the
--    most frequently occurring items occur first in each sublist.
sortByOccurrenceCount
    :: (Eq b, Ord b, Eq c, Ord c)
    => (a -> c) -- ^ The initial list is split into sublists on this property.
                --   Ie. this property is the same for all items within a sublist
                --    of the initial list, but distinct between items in different
                --    sublists.
    -> (a -> b) -- ^ Sort by this property
    -> [[a]]    -- ^ Initial list
    -> [[a]]
sortByOccurrenceCount distinct prop =
    splitOn distinct .
    concat .
    sortOnDesc length .
    groupBy (\a1 a2 -> prop a1 == prop a2) .
    sortOn prop .
    concat
  where
    splitOn prop' = Map.elems . foldr (newListOrCons prop') (Map.empty :: Map.Map c [a])
    newListOrCons prop' item map =
        let key = prop' item
        in maybe (Map.insert key [item] map)
                 (\list -> Map.insert key (item : list) map)
                 (Map.lookup key map)

-- | Sort descending
sortOnDesc :: Ord b => (a -> b) -> [a] -> [a]
sortOnDesc f = sortOn (Down . f)
