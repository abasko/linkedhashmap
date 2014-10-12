module Main where

import Criterion.Types
import Criterion.Main
import Control.DeepSeq
import Data.List
import Data.Maybe

import qualified Data.HashMap.Strict as M
import qualified Data.LinkedHashSet as LS
import qualified Data.LinkedHashMap.Seq as LM1
import qualified Data.LinkedHashMap.IntMap as LM2

deep :: NFData a => a -> a
deep a = deepseq a a

keys :: [Int]
keys = reverse $ take 100000 $ [0..]

rkeys :: [Int]
rkeys = reverse $ keys

intIntPairs :: [(Int, Int)]
intIntPairs = deep $ zip keys [0..]

insertAll :: (Int -> Int -> m -> m) -> [(Int, Int)] -> m -> m
insertAll ins kvs m0 = foldl' (\m (k, v) -> ins k v m) m0 kvs

deleteAll :: (Int -> m -> m) -> [Int] -> m -> m
deleteAll del ks m0 = foldl' (\m k -> del k m) m0 ks

lookupAll :: (Int -> m -> Maybe Int) -> [Int] -> m -> [Int]
lookupAll look ks m0 = map (\k -> fromJust $ look k m0) ks

setInsertAll :: (Int -> s -> s) -> [Int] -> s -> s
setInsertAll ins ks s0 = foldl' (\s k -> ins k s) s0 ks

setLookupAll :: [Int] -> LS.LinkedHashSet Int -> [Bool]
setLookupAll ks s0 = map (\k -> LS.member k s0) ks

benchmarks :: [Benchmark]
benchmarks = [bgroup "fromList" [
                          bench "HashMap.Strict" $ nf M.fromList intIntPairs,
                          bench "LinkedHashMap.Seq" $ nf LM1.fromList intIntPairs,
                          bench "LinkedHashMap.IntMap" $ nf LM2.fromList intIntPairs,
                          bench "LinkedHashSet" $ nf LS.fromList keys
                         ],
              bgroup "insert" [
                          bench "HashMap.Strict" $ nf (insertAll M.insert intIntPairs) M.empty,
                          bench "LinkedHashMap.Seq" $ nf (insertAll LM1.insert intIntPairs) LM1.empty,
                          bench "LinkedHashMap.IntMap" $ nf (insertAll LM2.insert intIntPairs) LM2.empty,
                          bench "LinkedHashSet" $ nf (setInsertAll LS.insert keys) LS.empty
                         ],
              bgroup "toList" [
                          bench "HashMap.Strict" $ nf M.toList m0,
                          bench "LinkedHashMap.Seq" $ nf LM1.toList m1,
                          bench "LinkedHashMap.IntMap" $ nf LM2.toList m2,
                          bench "LinkedHashSet" $ nf LS.toList s1
                         ],
              bgroup "lookup" [
                          bench "HashMap.Strict" $ nf (lookupAll M.lookup rkeys) m0,
                          bench "LinkedHashMap.Seq" $ nf (lookupAll LM1.lookup rkeys) m1,
                          bench "LinkedHashMap.IntMap" $ nf (lookupAll LM2.lookup rkeys) m2,
                          bench "LinkedHashSet" $ nf (setLookupAll rkeys) s1
                         ],
              bgroup "delete" [ 
                          bench "HashMap.Strict" $ nf (deleteAll M.delete rkeys) m0,
                          bench "LinkedHashMap.Seq" $ nf (deleteAll LM1.delete rkeys) m1,
                          bench "LinkedHashMap.IntMap" $ nf (deleteAll LM2.delete rkeys) m2,
                          bench "LinkedHashSet" $ nf (deleteAll LS.delete rkeys) s1
                         ]
             ]
             where 
               m0 = deep $ M.fromList intIntPairs
               m1 = deep $ LM1.fromList intIntPairs
               m2 = deep $ LM2.fromList intIntPairs
               s1 = deep $ LS.fromList keys

myConfig :: Config
myConfig = defaultConfig {
             resamples  = 1000,
             reportFile = Just "report.html"
           }

report :: IO ()
report = defaultMainWith myConfig benchmarks

main :: IO ()
main = defaultMain benchmarks
