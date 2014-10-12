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
    ) 
where

import Prelude hiding (null, lookup)
import Data.IORef
import System.IO.Unsafe
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData(rnf))
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap.Strict as IM

newtype Entry a = Entry { unEntry :: (Int, a) } deriving (Show)

instance Eq a => Eq (Entry a) where
    (Entry (_, a)) == (Entry (_, b)) = a == b

data LinkedHashMap k v = LinkedHashMap (M.HashMap k (Entry v)) (IM.IntMap (k, v)) (IORef Int)

instance (Show k, Show v) => Show (LinkedHashMap k v) where
    showsPrec d m@(LinkedHashMap _ _ _) = showParen (d > 10) $
      showString "fromList " . shows (toList m)

newCounter :: Int -> IORef Int
newCounter n = unsafePerformIO (newIORef n)

getCounter :: IORef Int -> Int
getCounter rn = unsafePerformIO $ readIORef rn

incCounter :: IORef Int -> Int
incCounter rn = unsafePerformIO $ atomicModifyIORef rn $ \n -> (n + 1, n + 1)

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> LinkedHashMap k v -> Maybe v
lookup k0 (LinkedHashMap m0 _ _) = case M.lookup k0 m0 of
                                     Just (Entry (_, v)) -> Just v
                                     Nothing -> Nothing
{-# INLINABLE lookup #-}

-- | /O(1)/ Construct an empty map.
empty :: LinkedHashMap k v
empty = LinkedHashMap M.empty IM.empty (newCounter minBound)

-- | /O(1)/ Construct a map with a single element.
singleton :: (Eq k, Hashable k) => k -> v -> LinkedHashMap k v
singleton k v = fromList [(k, v)]

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
insert k v (LinkedHashMap m s rn) = s' `seq` LinkedHashMap m' s' rn
  where 
    m' = M.insert k (Entry (n', v)) m
    s' = IM.insert n' (k, v) s
    n' = case M.lookup k m of
          Just (Entry (n, _)) -> n
          Nothing -> incCounter rn
{-# INLINABLE insert #-}

-- | /O(log n)/ Remove the mapping for the specified key from this map
-- if present.
delete :: (Eq k, Hashable k) => k -> LinkedHashMap k v -> LinkedHashMap k v
delete k0 (LinkedHashMap m s rn) = LinkedHashMap (M.delete k0 m) (case M.lookup k0 m of
                                                                    Nothing -> s
                                                                    Just (Entry (i, _)) -> IM.delete i s) rn

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

instance (NFData a) => NFData (Entry a) where
    rnf (Entry a) = rnf a

instance (NFData k, NFData v) => NFData (LinkedHashMap k v) where
    rnf (LinkedHashMap m s _) = rnf m `seq` rnf s
