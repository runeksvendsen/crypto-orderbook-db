{-# LANGUAGE OverloadedLists #-}
module CryptoDepth.OrderBook.Db.Internal.Util
( mergeSamePricedOrders
)
where

import qualified OrderBook.Types                    as OB
import           GHC.Exts                           (IsList(..))


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
