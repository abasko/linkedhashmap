module Data.LinkedHashSet
    (
     LinkedHashSet

    -- * Construction
    , empty
    , singleton

    -- * Combine
    , union
    , unions

    -- * Basic interface
    , null
    , size
    , member
    , insert
    , delete

    -- * Transformations
    , map

      -- * Difference and intersection
    -- , difference
    -- , intersection

    -- * Folds
    -- , foldl'
    -- , foldr

    -- * Filter
    -- , filter

    -- ** Lists
    , toList
    , fromList
    ) where

import Prelude hiding (null, lookup, map)
import Control.DeepSeq (NFData(rnf))
import Data.Hashable (Hashable)
import qualified Data.List as L
import qualified Data.LinkedHashMap.Seq as M

-- | A set of values.  A set cannot contain duplicate values.
newtype LinkedHashSet a = LinkedHashSet {
      asMap :: M.LinkedHashMap a ()
    }

instance (Show a) => Show (LinkedHashSet a) where
    showsPrec d m = showParen (d > 10) $
      showString "fromList " . shows (toList m)

-- | /O(1)/ Construct an empty set.
empty :: LinkedHashSet a
empty = LinkedHashSet M.empty

-- | /O(1)/ Construct a set with a single element.
singleton :: (Eq a, Hashable a) => a -> LinkedHashSet a
singleton a = LinkedHashSet (M.singleton a ())
{-# INLINABLE singleton #-}

-- | /O(m*log n)/ Construct a set containing all elements from both sets, n - size of first map.
union :: (Eq a, Hashable a) => LinkedHashSet a -> LinkedHashSet a -> LinkedHashSet a
union s1 s2 = LinkedHashSet $ M.union (asMap s1) (asMap s2)
{-# INLINE union #-}

-- | Construct a set containing all elements from a list of sets.
unions :: (Eq a, Hashable a) => [LinkedHashSet a] -> LinkedHashSet a
unions = L.foldl' union empty
{-# INLINE unions #-}

-- | /O(1)/ Return 'True' if this set is empty, 'False' otherwise.
null :: LinkedHashSet a -> Bool
null = M.null . asMap
{-# INLINE null #-}

-- | /O(1)/ Return the number of elements in this set.
size :: LinkedHashSet a -> Int
size = M.size . asMap
{-# INLINE size #-}

-- | /O(min(n,W))/ Return 'True' if the given value is present in this
-- set, 'False' otherwise.
member :: (Eq a, Hashable a) => a -> LinkedHashSet a -> Bool
member a s = case M.lookup a (asMap s) of
               Just _ -> True
               _      -> False
{-# INLINABLE member #-}

-- | /O(min(n,W))/ Add the specified value to this set.
insert :: (Eq a, Hashable a) => a -> LinkedHashSet a -> LinkedHashSet a
insert a = LinkedHashSet . M.insert a () . asMap
{-# INLINABLE insert #-}

-- | /O(min(n,W))/ Remove the specified value from this set if
-- present.
delete :: (Eq a, Hashable a) => a -> LinkedHashSet a -> LinkedHashSet a
delete a = LinkedHashSet . M.delete a . asMap
{-# INLINABLE delete #-}

-- | /O(n)/ Transform this set by applying a function to every value.
-- The resulting set may be smaller than the source.
map :: (Hashable b, Eq b) => (a -> b) -> LinkedHashSet a -> LinkedHashSet b
map f = fromList . L.map f . toList
{-# INLINE map #-}

-- -- | /O(n)/ Difference of two sets. Return elements of the first set
-- -- not existing in the second.
-- difference :: (Eq a, Hashable a) => LinkedHashSet a -> LinkedHashSet a -> LinkedHashSet a
-- difference (LinkedHashSet a) (LinkedHashSet b) = LinkedHashSet (M.difference a b)
-- {-# INLINABLE difference #-}

-- -- | /O(n)/ Intersection of two sets. Return elements present in both
-- -- the first set and the second.
-- intersection :: (Eq a, Hashable a) => LinkedHashSet a -> LinkedHashSet a -> LinkedHashSet a
-- intersection (LinkedHashSet a) (LinkedHashSet b) = LinkedHashSet (M.intersection a b)
-- {-# INLINABLE intersection #-}

-- -- | /O(n)/ Reduce this set by applying a binary operator to all
-- -- elements, using the given starting value (typically the
-- -- left-identity of the operator).  Each application of the operator
-- -- is evaluated before before using the result in the next
-- -- application.  This function is strict in the starting value.
-- foldl' :: (a -> b -> a) -> a -> LinkedHashSet b -> a
-- foldl' f z0 = M.foldlWithKey' g z0 . asMap
--   where g z k _ = f z k
-- {-# INLINE foldl' #-}

-- -- | /O(n)/ Reduce this set by applying a binary operator to all
-- -- elements, using the given starting value (typically the
-- -- right-identity of the operator).
-- foldr :: (b -> a -> a) -> a -> LinkedHashSet b -> a
-- foldr f z0 = foldrWithKey g z0 . asMap
--   where g k _ z = f k z
-- {-# INLINE foldr #-}

-- -- | /O(n)/ Filter this set by retaining only elements satisfying a
-- -- predicate.
-- filter :: (a -> Bool) -> LinkedHashSet a -> LinkedHashSet a
-- filter p = LinkedHashSet . H.filterWithKey q . asMap
--   where q k _ = p k
-- {-# INLINE filter #-}

-- | /O(n)/ Return a list of this set's elements.  The list is produced lazily.
toList :: LinkedHashSet a -> [a]
toList t = L.map (\(k, _) -> k) $ M.toList (asMap t)
{-# INLINE toList #-}

-- | /O(n*min(W, n))/ Construct a set from a list of elements.
fromList :: (Eq a, Hashable a) => [a] -> LinkedHashSet a
fromList as = LinkedHashSet $ M.fromList $ zip as $ cycle [()]
{-# INLINE fromList #-}

instance (NFData a) => NFData (LinkedHashSet a) where
  rnf = rnf . asMap
