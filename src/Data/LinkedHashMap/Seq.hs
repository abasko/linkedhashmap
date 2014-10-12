{-# LANGUAGE BangPatterns #-}
module Data.LinkedHashMap.Seq 
    (
      LinkedHashMap(..)

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , null
    , size
    , member
    , lookup
    , lookupDefault
    , (!)
    , insert
    -- , insertWith
    , delete
    -- , adjust

      -- * Combine
      -- ** Union
    -- , union
    -- , unionWith
    -- , unions

      -- * Transformations
    , map
    -- , mapWithKey
    -- , traverseWithKey

      -- * Difference and intersection
    -- , difference
    -- , intersection
    -- , intersectionWith

      -- * Folds
    -- , foldl'
    -- , foldlWithKey'
    -- , foldr
    -- , foldrWithKey

      -- * Filter
    -- , filter
    -- , filterWithKey

      -- * Conversions
    , keys
    , elems

      -- ** Lists
    , toList
    , fromList
    -- , fromListWith
    , pack
    ) where

import Prelude hiding (null, lookup)
import Data.Maybe
import Control.DeepSeq (NFData(rnf))
import Data.Hashable (Hashable)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as M

newtype Entry a = Entry { unEntry :: (Int, a) } deriving (Show)

instance Eq a => Eq (Entry a) where
    (Entry (_, a)) == (Entry (_, b)) = a == b

-- Contains HashMap, ordered keys Seq and number of not deleted keys in a sequence (size of HashMap)
data LinkedHashMap k v = LinkedHashMap (M.HashMap k (Entry v)) (Seq (Maybe (k, v))) !Int

instance (Show k, Show v) => Show (LinkedHashMap k v) where
    showsPrec d m@(LinkedHashMap _ _ _) = showParen (d > 10) $
      showString "fromList " . shows (toList m)

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> LinkedHashMap k v -> Maybe v
lookup k0 (LinkedHashMap m0 _ _) = case M.lookup k0 m0 of
                                     Just (Entry (_, v)) -> Just v
                                     Nothing -> Nothing
{-# INLINABLE lookup #-}

-- | /O(n*log n)/ Construct a map with the supplied mappings.  If the
-- list contains duplicate mappings, the later mappings take
-- precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> LinkedHashMap k v
fromList ps = LinkedHashMap m' s' len'
  where
    m0 = M.fromList $ map (\(i, (k, v)) -> (k, Entry (i, v))) $ zip [0..] ps
    s0 = S.fromList $ map (\(k, v) -> Just (k, v)) ps
    len = M.size m0
    (m', s', len') = if len == S.length s0
                     then (m0, s0, len)
                     else F.foldl' skipDups (m0, S.empty, 0) s0
    skipDups (m, s, n) jkv@(Just (k, _)) 
      | n == ix = (m, s |> jkv, n + 1)
      | n > ix = (m, s, n)
      | otherwise = (M.insert k (Entry (n, v)) m, s |> Just (k, v), n + 1)
      where 
        (ix, v) = unEntry $ fromJust $ M.lookup k m
    skipDups _ _ = error "Data.LinkedHashMap.Seq invariant violated"

-- | /O(n)/ Return a list of this map's elements.  The list is produced lazily.
toList ::LinkedHashMap k v -> [(k, v)]
toList (LinkedHashMap _ s _) = catMaybes (F.toList s)
{-# INLINABLE toList #-}

-- | /O(log n)/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> LinkedHashMap k v -> LinkedHashMap k v
insert k !v (LinkedHashMap m s n) = LinkedHashMap m' s' n'
  where 
    m' = M.insert k (Entry (ix', v)) m
    (s', ix', n') = case M.lookup k m of
                      Just (Entry (ix, _)) -> (s, ix, n)
                      Nothing -> (s |> Just (k, v), S.length s, n+1)
{-# INLINABLE insert #-}

pack :: (Eq k, Hashable k) => LinkedHashMap k v -> LinkedHashMap k v
pack = fromList . toList

-- | /O(log n)/ Remove the mapping for the specified key from this map
-- if present.
delete :: (Eq k, Hashable k) => k -> LinkedHashMap k v -> LinkedHashMap k v
delete k0 (LinkedHashMap m s n) = if S.length s `div` 2 >= n 
                                  then pack lhm 
                                  else lhm
  where
    lhm = LinkedHashMap m' s' n'
    (m', s', n') = case M.lookup k0 m of
                     Nothing -> (m, s, n)
                     Just (Entry (i, _)) -> (M.delete k0 m, S.update i Nothing s, n-1)
                                           
-- | /O(1)/ Construct an empty map.
empty :: LinkedHashMap k v
empty = LinkedHashMap M.empty S.empty 0

-- | /O(1)/ Construct a map with a single element.
singleton :: (Eq k, Hashable k) => k -> v -> LinkedHashMap k v
singleton k v = fromList [(k, v)]

-- | /O(1)/ Return 'True' if this map is empty, 'False' otherwise.
null :: LinkedHashMap k v -> Bool
null (LinkedHashMap m _ _) = M.null m

-- | /O(log n)/ Return 'True' if the specified key is present in the
-- map, 'False' otherwise.
member :: (Eq k, Hashable k) => k -> LinkedHashMap k a -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _  -> True
{-# INLINABLE member #-}

-- | /O(1)/ Return the number of key-value mappings in this map.
size :: LinkedHashMap k v -> Int
size (LinkedHashMap _ _ n) = n

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or the default value if this map contains no mapping for the key.
lookupDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> LinkedHashMap k v -> v
lookupDefault def k t = case lookup k t of
    Just v -> v
    _      -> def
{-# INLINABLE lookupDefault #-}

-- | /O(log n)/ Return the value to which the specified key is mapped.
-- Calls 'error' if this map contains no mapping for the key.
(!) :: (Eq k, Hashable k) => LinkedHashMap k v -> k -> v
(!) m k = case lookup k m of
    Just v  -> v
    Nothing -> error "Data.LinkedHashMap.Seq.(!): key not found"
{-# INLINABLE (!) #-}

-- | /O(n)/ Return a list of this map's keys.  The list is produced
-- lazily.
keys :: (Eq k, Hashable k) => LinkedHashMap k v -> [k]
keys m = map (\(k, _) -> k) $ toList m
{-# INLINE keys #-}

-- | /O(n)/ Return a list of this map's values.  The list is produced
-- lazily.
elems :: (Eq k, Hashable k) => LinkedHashMap k v -> [v]
elems m = map (\(_, v) -> v) $ toList m
{-# INLINE elems #-}

instance (NFData a) => NFData (Entry a) where
    rnf (Entry a) = rnf a

instance (NFData k, NFData v) => NFData (LinkedHashMap k v) where
    rnf (LinkedHashMap m s _) = rnf m `seq` rnf s
