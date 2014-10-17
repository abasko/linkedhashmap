{-# LANGUAGE BangPatterns #-}
module Data.LinkedHashMap.IntMap 
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
    , insertWith
    , delete
    , adjust

      -- * Combine
      -- ** Union
    , union
    , unionWith
    , unions

      -- * Transformations
    , map
    , mapWithKey
    , traverseWithKey

      -- * Difference and intersection
    , difference
    , intersection
    , intersectionWith

      -- * Folds
    , foldl'
    , foldlWithKey'
    , foldr
    , foldrWithKey

      -- * Filter
    , filter
    , filterWithKey

      -- * Conversions
    , keys
    , elems

      -- ** Lists
    , toList
    , fromList
    , fromListWith
    ) 
where

import Prelude hiding (foldr, map, null, lookup, filter)
import Data.Maybe
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData(rnf))
import Control.Applicative ((<$>), Applicative)
import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap.Strict as IM

newtype Entry a = Entry { unEntry :: (Int, a) } deriving (Show)

data LinkedHashMap k v = LinkedHashMap (M.HashMap k (Entry v)) (IM.IntMap (k, v)) Int

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
    Nothing -> error "Data.LinkedHashMap.IntMap.(!): key not found"
{-# INLINABLE (!) #-}

-- | /O(log n)/ Remove the mapping for the specified key from this map
-- if present.
delete :: (Eq k, Hashable k) => k -> LinkedHashMap k v -> LinkedHashMap k v
delete k0 (LinkedHashMap m s maxn) = LinkedHashMap (M.delete k0 m) (case M.lookup k0 m of
                                                                      Nothing -> s
                                                                      Just (Entry (i, _)) -> IM.delete i s) maxn

-- | /O(1)/ Construct an empty map.
empty :: LinkedHashMap k v
empty = LinkedHashMap M.empty IM.empty minBound

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
size (LinkedHashMap _ s _) = IM.size s

-- | /O(n)/ Return a list of this map's keys.  The list is produced
-- lazily.
keys :: (Eq k, Hashable k) => LinkedHashMap k v -> [k]
keys m = fmap (\(k, _) -> k) $ toList m
{-# INLINE keys #-}

-- | /O(n)/ Return a list of this map's values.  The list is produced
-- lazily.
elems :: (Eq k, Hashable k) => LinkedHashMap k v -> [v]
elems m = fmap (\(_, v) -> v) $ toList m
{-# INLINE elems #-}

-- | /O(n*log n)/ Construct a map with the supplied mappings.  If the
-- list contains duplicate mappings, the later mappings take
-- precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> LinkedHashMap k v
fromList = F.foldl' (\m (k, v) -> insert k v m) empty
{-# INLINABLE fromList #-}

-- | /O(n)/ Return a list of this map's elements.
toList :: LinkedHashMap k v -> [(k, v)]
toList (LinkedHashMap _ s _) = IM.elems s
{-# INLINE toList #-}

-- | /O(log n)/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> LinkedHashMap k v -> LinkedHashMap k v
insert k v (LinkedHashMap m s maxn) = s' `seq` LinkedHashMap m' s' maxn'
  where 
    m' = M.insert k (Entry (n', v)) m
    s' = IM.insert n' (k, v) s
    (n', maxn') = case M.lookup k m of
                    Just (Entry (n, _)) -> (n, maxn)
                    Nothing -> let newn = maxn + 1 in (newn, newn)
{-# INLINABLE insert #-}

-- | /O(log n)/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> LinkedHashMap k v -> LinkedHashMap k v
insertWith f k v0 (LinkedHashMap m s maxn) = s' `seq` LinkedHashMap m' s' maxn'
  where
    m' = M.insert k (Entry (n', v')) m
    s' = IM.insert n' (k, v') s
    (n', v', maxn') = case M.lookup k m of
                        Just (Entry (n, v)) -> (n, f v0 v, maxn)
                        Nothing -> let newn = maxn + 1 in (newn, v0, newn)

-- | /O(log n)/ Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
adjust :: (Eq k, Hashable k) => (v -> v) -> k -> LinkedHashMap k v -> LinkedHashMap k v
adjust f k (LinkedHashMap m s maxn) = LinkedHashMap m' s' maxn
  where
    m' = M.adjust f' k m
    f' (Entry (ix, v)) = Entry (ix, f v)
    s' = case M.lookup k m' of
           Just (Entry (ix, v)) -> IM.insert ix (k, v) s
           Nothing -> s

-- | /O(m*log n)/ The union of two maps, n - size of the first map. If a key occurs in both maps,
-- the mapping from the first will be the mapping in the result.
union :: (Eq k, Hashable k) => LinkedHashMap k v -> LinkedHashMap k v -> LinkedHashMap k v
union = unionWith const
{-# INLINABLE union #-}

-- | /O(m*log n)/ The union of two maps, n - size of the first map.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the result.
unionWith :: (Eq k, Hashable k) => (v -> v -> v) -> LinkedHashMap k v -> LinkedHashMap k v
          -> LinkedHashMap k v
unionWith f m1 m2 = m'
  where
    m' = F.foldl' (\m (k, v) -> insertWith (flip f) k v m) m1 $ toList m2

-- | Construct a set containing all elements from a list of sets.
unions :: (Eq k, Hashable k) => [LinkedHashMap k v] -> LinkedHashMap k v
unions = F.foldl' union empty
{-# INLINE unions #-}

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> LinkedHashMap k v1 -> LinkedHashMap k v2
map f = mapWithKey (const f)
{-# INLINE map #-}

-- | /O(n)/ Transform this map by applying a function to every value.
mapWithKey :: (k -> v1 -> v2) -> LinkedHashMap k v1 -> LinkedHashMap k v2
mapWithKey f (LinkedHashMap m s maxn) = (LinkedHashMap m' s' maxn)
  where
    m' = M.mapWithKey f' m
    s' = fmap f'' s
    f' k (Entry (ix, v1)) = Entry (ix, f k v1)
    f'' (k, v1) = (k, f k v1)

-- | /O(n*log(n))/ Transform this map by accumulating an Applicative result
-- from every value.
traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> LinkedHashMap k v1
                -> f (LinkedHashMap k v2)
traverseWithKey f (LinkedHashMap m0 s0 maxn) = (\s -> LinkedHashMap (M.map (getV2 s) m0) s maxn) <$> s'
  where
    s' = T.traverse f' s0
    f' (k, v1) = (\v -> (k, v)) <$> f k v1
    getV2 s (Entry (ix, _)) = let (_, v2) = fromJust $ IM.lookup ix s in Entry (ix, v2)
{-# INLINE traverseWithKey #-}

-- | /O(n*log m)/ Difference of two maps. Return elements of the first map
-- not existing in the second.
difference :: (Eq k, Hashable k) => LinkedHashMap k v -> LinkedHashMap k w -> LinkedHashMap k v
difference a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> insert k v m
                 _       -> m
{-# INLINABLE difference #-}

-- | /O(n*log m)/ Intersection of two maps. Return elements of the first
-- map for keys existing in the second.
intersection :: (Eq k, Hashable k) => LinkedHashMap k v -> LinkedHashMap k w -> LinkedHashMap k v
intersection a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Just _ -> insert k v m
                 _      -> m
{-# INLINABLE intersection #-}

-- | /O(n+m)/ Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWith :: (Eq k, Hashable k) => (v1 -> v2 -> v3) -> LinkedHashMap k v1
                 -> LinkedHashMap k v2 -> LinkedHashMap k v3
intersectionWith f a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Just w -> insert k (f v w) m
                 _      -> m
{-# INLINABLE intersectionWith #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldl' :: (a -> v -> a) -> a -> LinkedHashMap k v -> a
foldl' f b0 (LinkedHashMap _ s _) = F.foldl' f' b0 s
  where
    f' b (_, v) = f b v

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldr :: (v -> a -> a) -> a -> LinkedHashMap k v -> a
foldr = F.foldr
{-# INLINE foldr #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldlWithKey' :: (a -> k -> v -> a) -> a -> LinkedHashMap k v -> a
foldlWithKey' f b0 (LinkedHashMap _ s _) = F.foldl' f' b0 s
  where
    f' b (k, v) = f b k v
{-# INLINE foldlWithKey' #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (k -> v -> a -> a) -> a -> LinkedHashMap k v -> a
foldrWithKey f b0 (LinkedHashMap _ s _) = F.foldr f' b0 s
  where
    f' (k, v) b = f k v b

-- | /O(n*log(n))/ Filter this map by retaining only elements satisfying a
-- predicate.
filterWithKey :: (Eq k, Hashable k) => (k -> v -> Bool) -> LinkedHashMap k v -> LinkedHashMap k v
filterWithKey p m = fromList $ L.filter (uncurry p) $ toList m

-- | /O(n*log(n))/ Filter this map by retaining only elements which values
-- satisfy a predicate.
filter :: (Eq k, Hashable k) => (v -> Bool) -> LinkedHashMap k v -> LinkedHashMap k v
filter p = filterWithKey (\_ v -> p v)
{-# INLINE filter #-}

-- | /O(n*log n)/ Construct a map from a list of elements.  Uses
-- the provided function to merge duplicate entries.
fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> LinkedHashMap k v
fromListWith f = L.foldl' (\ m (k, v) -> insertWith f k v m) empty
{-# INLINE fromListWith #-}

instance (NFData a) => NFData (Entry a) where
    rnf (Entry a) = rnf a

instance (NFData k, NFData v) => NFData (LinkedHashMap k v) where
    rnf (LinkedHashMap m s _) = rnf m `seq` rnf s

instance Functor (LinkedHashMap k) where
    fmap = map

instance F.Foldable (LinkedHashMap k) where
    foldr f b0 (LinkedHashMap _ s _) = F.foldr f' b0 s
      where
        f' (_, v) b = f v b
        
instance T.Traversable (LinkedHashMap k) where
    traverse f = traverseWithKey (const f)
