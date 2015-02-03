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

    , pack
    ) where

import Prelude hiding (foldr, map, null, lookup, filter)
import Data.Maybe
import Control.Applicative ((<$>), Applicative(pure))
import Control.DeepSeq (NFData(rnf))
import Data.Hashable (Hashable)
import Data.Sequence (Seq, (|>)) 
import Data.Traversable (Traversable(..))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as L
import qualified Data.HashMap.Strict as M

data Entry a = Entry {-# UNPACK #-}!Int a deriving (Show)
data MaybePair k v = NothingPair | JustPair k v deriving (Show)

-- Contains HashMap, ordered keys Seq and number of not deleted keys in a sequence (size of HashMap)
data LinkedHashMap k v = LinkedHashMap (M.HashMap k (Entry v)) (Seq (MaybePair k v)) {-# UNPACK #-}!Int

instance (Show k, Show v) => Show (LinkedHashMap k v) where
    showsPrec d m@(LinkedHashMap _ _ _) = showParen (d > 10) $
      showString "fromList " . shows (toList m)

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> LinkedHashMap k v -> Maybe v
lookup k0 (LinkedHashMap m0 _ _) = case M.lookup k0 m0 of
                                     Just (Entry _ v) -> Just v
                                     Nothing -> Nothing
{-# INLINABLE lookup #-}

-- | /O(n*log n)/ Construct a map from a list of elements.  Uses
-- the provided function to merge duplicate entries.
fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> LinkedHashMap k v
fromListWith f = L.foldl' (\ m (k, v) -> insertWith f k v m) empty
{-# INLINE fromListWith #-}

-- | /O(n*log n)/ Construct a map with the supplied mappings.  If the
-- list contains duplicate mappings, the later mappings take
-- precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> LinkedHashMap k v
fromList ps = LinkedHashMap m' s' len'
  where
    m0 = M.fromList $ L.map (\(i, (k, v)) -> (k, Entry i v)) $ zip [0..] ps
    s0 = S.fromList $ L.map (\(k, v) -> JustPair k v) ps
    len = M.size m0
    (m', s', len') = if len == S.length s0
                     then (m0, s0, len)
                     else F.foldl' skipDups (m0, S.empty, 0) s0
    skipDups (m, s, n) jkv@(JustPair k _) 
      | n == ix = (m, s |> jkv, n + 1)
      | n > ix = (m, s, n)
      | otherwise = (M.insert k (Entry n v) m, s |> JustPair k v, n + 1)
      where 
        Entry ix v = fromJust $ M.lookup k m
    skipDups _ _ = error "Data.LinkedHashMap.Seq invariant violated"

-- | /O(n)/ Return a list of this map's elements.  The list is produced lazily.
toList ::LinkedHashMap k v -> [(k, v)]
toList (LinkedHashMap _ s _) = [(k, v) | JustPair k v <- F.toList s] 
{-# INLINABLE toList #-}

-- | /O(log n)/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> LinkedHashMap k v -> LinkedHashMap k v
insert k !v (LinkedHashMap m s n) = LinkedHashMap m' s' n'
  where 
    m' = M.insert k (Entry ix' v) m
    (s', ix', n') = case M.lookup k m of
                      Just (Entry ix _) -> (S.update ix (JustPair k v) s, ix, n)
                      Nothing -> (s |> JustPair k v, S.length s, n+1)
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
                     Just (Entry i _) -> (M.delete k0 m, S.update i NothingPair s, n-1)
                                           
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
keys m = L.map (\(k, _) -> k) $ toList m
{-# INLINE keys #-}

-- | /O(n)/ Return a list of this map's values.  The list is produced
-- lazily.
elems :: (Eq k, Hashable k) => LinkedHashMap k v -> [v]
elems m = L.map (\(_, v) -> v) $ toList m
{-# INLINE elems #-}

-- | /O(log n)/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> LinkedHashMap k v -> LinkedHashMap k v
insertWith f k v (LinkedHashMap m s n) = LinkedHashMap m' s' n'
  where
    m' = M.insertWith f' k v' m
    f' (Entry _ v1) (Entry ix v2) = Entry ix $ f v1 v2
    slen = S.length s
    v' = Entry slen v
    Entry ixnew vnew = fromJust $ M.lookup k m'
    (s', n') = if ixnew == slen 
               then (s |> JustPair k vnew, n + 1)
               else (S.update ixnew (JustPair k vnew) s, n)

-- | /O(log n)/ Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
adjust :: (Eq k, Hashable k) => (v -> v) -> k -> LinkedHashMap k v -> LinkedHashMap k v
adjust f k (LinkedHashMap m s n) = LinkedHashMap m' s' n
  where
    m' = M.adjust f' k m
    f' (Entry ix v) = Entry ix $ f v
    s' = case M.lookup k m' of
           Just (Entry ix v) -> S.update ix (JustPair k v) s
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
mapWithKey f (LinkedHashMap m s n) = (LinkedHashMap m' s' n)
  where
    m' = M.mapWithKey f' m
    s' = fmap f'' s
    f' k (Entry ix v1) = Entry ix $ f k v1
    f'' (JustPair k v1) = JustPair k $ f k v1
    f'' _  = NothingPair

-- | /O(n*log(n))/ Transform this map by accumulating an Applicative result
-- from every value.
traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> LinkedHashMap k v1
                -> f (LinkedHashMap k v2)
traverseWithKey f (LinkedHashMap m0 s0 n) = (\s -> LinkedHashMap (M.map (getV2 s) m0) s n) <$> s'
  where
    s' = T.traverse f' s0
    f' (JustPair k v1) = (\v -> JustPair k v) <$> f k v1
    f' NothingPair = pure NothingPair
    getV2 s (Entry ix _) = let JustPair _ v2 = S.index s ix in Entry ix v2

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
    f' b (JustPair _ v) = f b v
    f' b _ = b

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
    f' b (JustPair k v) = f b k v
    f' b _ = b

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (k -> v -> a -> a) -> a -> LinkedHashMap k v -> a
foldrWithKey f b0 (LinkedHashMap _ s _) = F.foldr f' b0 s
  where
    f' (JustPair k v) b = f k v b
    f' _ b = b

-- | /O(n*log(n))/ Filter this map by retaining only elements satisfying a
-- predicate.
filterWithKey :: (Eq k, Hashable k) => (k -> v -> Bool) -> LinkedHashMap k v -> LinkedHashMap k v
filterWithKey p m = fromList $ L.filter (uncurry p) $ toList m

-- | /O(n*log(n))/ Filter this map by retaining only elements which values
-- satisfy a predicate.
filter :: (Eq k, Hashable k) => (v -> Bool) -> LinkedHashMap k v -> LinkedHashMap k v
filter p = filterWithKey (\_ v -> p v)
{-# INLINE filter #-}

instance (NFData a) => NFData (Entry a) where
    rnf (Entry _ a) = rnf a

instance (NFData a, NFData b) => NFData (MaybePair a b) where
    rnf (JustPair a b) = rnf a `seq` rnf b
    rnf NothingPair = ()

instance (NFData k, NFData v) => NFData (LinkedHashMap k v) where
    rnf (LinkedHashMap m s _) = rnf m `seq` rnf s

instance Functor (LinkedHashMap k) where
    fmap = map

instance F.Foldable (LinkedHashMap k) where
    foldr f b0 (LinkedHashMap _ s _) = F.foldr f' b0 s
      where
        f' (JustPair _ v) b = f v b
        f' _ b = b
        
instance T.Traversable (LinkedHashMap k) where
    traverse f = traverseWithKey (const f)
