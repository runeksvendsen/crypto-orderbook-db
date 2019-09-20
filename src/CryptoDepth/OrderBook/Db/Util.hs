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


-- | Merge adjacent, same-priced orders
mergeSamePricedOrders
    :: OB.OrderBook venue base quote
    -- ^ Contains buy and sell orders sorted by price
    -> OB.OrderBook venue base quote
    -- ^ All same-priced buy/sell orders have been merged into one buy/sell order
mergeSamePricedOrders book =
    OB.OrderBook
        { obBids = OB.BuySide . asList mergeSamePrice . OB.buySide . OB.obBids $ book
        , obAsks = OB.SellSide . asList mergeSamePrice . OB.sellSide . OB.obAsks $ book
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
    combine tryMergeOrders
  where
    mergeOrders order1 order2 = order1
        { OB.oQuantity = OB.oQuantity order1 + OB.oQuantity order2 }
    tryMergeOrders order1 order2 =
        if OB.oPrice order1 == OB.oPrice order2
            then Just (mergeOrders order1 order2)
            else Nothing

-- | Combine adjacent list items.
--
--   Invariants:
--      "combine (const $ const Nothing) = id"
--      "combine (const $ const $ Just value) _ = [value]"
--      "combine f >>> groupBy (f >>> isJust) >>> all (length >>> (== 1)) = const True"
combine
    :: (a -> a -> Maybe a)
    -- ^ If the two adjacent list items can be combined,
    --  return 'Just' of an item that is the combination of the two items,
    --  otherwise 'Nothing'.
    -> [a]
    -> [a]
combine f =
    reverse . foldl (\accum item -> combine' accum item) []
  where
    combine' [] item = [item]
    combine' accumList@(newestItem : remainingItems) item =
        case f newestItem item of
            Just combinedA -> combinedA : remainingItems
            Nothing        -> item : accumList

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
    sortOnDesc :: Ord b => (a -> b) -> [a] -> [a]
    sortOnDesc f = sortOn (Down . f)
    splitOn prop' = Map.elems . foldr (newListOrCons prop') (Map.empty :: Map.Map c [a])
    newListOrCons prop' item map =
        let key = prop' item
        in maybe (Map.insert key [item] map)
                 (\list -> Map.insert key (item : list) map)
                 (Map.lookup key map)
