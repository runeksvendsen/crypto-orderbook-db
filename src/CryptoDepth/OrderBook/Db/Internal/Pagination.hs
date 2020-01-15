-- {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module CryptoDepth.OrderBook.Db.Internal.Pagination
(
)
where

import CryptoDepth.OrderBook.Db.Internal.Prelude

import qualified Text.Read                          as R
import qualified Data.Char                          as C
import qualified Data.Pagination                    as Page
import           Database.Beam.Query


data SortOrder = Ascending | Descending
    deriving (Eq, Show, Bounded, Enum, Ord, Generic)

instance Read SortOrder where
  readPrec =
    let fromLowerCase str
            | str `elem` ["ascending", "asc"  ] = return Ascending
            | str `elem` ["descending", "desc"] = return Descending
            | otherwise = R.pfail
    in do
        R.Ident str <- R.lexP
        fromLowerCase (map C.toLower str)

paginateQuery query ordering pagination = select $
    paginate'
        pagination
        (limitOffsetCountQuery query ordering)

limitOffsetCountQuery query ordering offset limit =
    withWindow_
        (\_ -> frame_ noPartition_ noOrder_ noBounds_)
        (\i _ -> (limitOffsetQuery i ordering offset limit, rowCount i))
        query

limitOffsetQuery query ordering offset limit =
    limit_ limit
        $ offset_ offset
        $ orderBy_ (\res -> ordering res) query

rowCount query =
    aggregate_ (const $ countAll_) query

paginate'
    :: (Functor m, Integral n)
    => Page.Pagination
    -> (n -> n -> m ([a], n)) -- offset limit
    -> m (Page.Paginated a)
paginate' = undefined
