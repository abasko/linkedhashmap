module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.State
import Control.Applicative ((<$>))

import qualified Data.LinkedHashMap.Seq as LM1
import qualified Data.LinkedHashMap.IntMap as LM2

test0_1 = LM1.fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "C")]
test0_2 = LM2.fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "C")]

test1_1 = LM1.insert 3 "ZZ" test0_1
test1_2 = LM2.insert 3 "ZZ" test0_2

test2_1 = LM1.insert (-10) "AA" $ LM1.insert 3 "ZZ" test0_1
test2_2 = LM2.insert (-10) "AA" $ LM2.insert 3 "ZZ" test0_2

test3_1 = LM1.delete 5 $ LM1.insert (-10) "AA" $ LM1.insert 3 "ZZ" test0_1
test3_2 = LM2.delete 5 $ LM2.insert (-10) "AA" $ LM2.insert 3 "ZZ" test0_2

test4_1 = LM1.delete 3 $ LM1.delete 5 $ LM1.insert (-10) "AA" $ LM1.insert 3 "ZZ" test0_1
test4_2 = LM2.delete 3 $ LM2.delete 5 $ LM2.insert (-10) "AA" $ LM2.insert 3 "ZZ" test0_2

test5_1 = LM1.delete 3 $ LM1.insert 3 "AA" $ LM1.insert 3 "ZZ" test0_1
test5_2 = LM2.delete 3 $ LM2.insert 3 "AA" $ LM2.insert 3 "ZZ" test0_2

test6_1 = LM1.fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1 :: Int,"AAAAA")]
test6_2 = LM2.fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1 :: Int,"AAAAA")]

test7_1 = LM1.fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1,"AA"), (5,"BB"), (7,"CC"),(7,"CCC")]
test7_2 = LM2.fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1,"AA"), (5,"BB"), (7,"CC"),(7,"CCC")]

test8_1 = LM1.fromList [(1 :: Int,"A"), (1, "B"), (1, "C"), (1, "D")]
test8_2 = LM2.fromList [(1 :: Int,"A"), (1, "B"), (1, "C"), (1, "D")]

test9_1 = LM1.fromList ([] :: [(Int, String)])
test9_2 = LM2.fromList ([] :: [(Int, String)])

test10_1 = LM1.insertWith (++) 5 "ZZZ_" test0_1
test10_2 = LM2.insertWith (++) 5 "ZZZ_" test0_2
test10_3 = LM1.insertWith (++) 11 "ZZZ_" test0_1
test10_4 = LM2.insertWith (++) 11 "ZZZ_" test0_2

test11_1 = LM1.adjust (\v0 -> v0 ++ "_adjusted") 5 test0_1
test11_2 = LM2.adjust (\v0 -> v0 ++ "_adjusted") 5 test0_2
test11_3 = LM1.adjust (\v0 -> v0 ++ "_adjusted") 123 test0_1
test11_4 = LM2.adjust (\v0 -> v0 ++ "_adjusted") 123 test0_2

t0_1 = LM1.fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D")]
t1_1 = LM1.fromList [(2 :: Int,"2A"), (3, "2B"), (7, "2C"), (-6, "2D")]
t2_1 = LM1.fromList [(0 :: Int,"3A"), (5, "3B"), (17, "3C"), (-6, "3D")]

t0_2 = LM2.fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D")]
t1_2 = LM2.fromList [(2 :: Int,"2A"), (3, "2B"), (7, "2C"), (-6, "2D")]
t2_2 = LM2.fromList [(0 :: Int,"3A"), (5, "3B"), (17, "3C"), (-6, "3D")]

test12_1 = LM1.union t0_1 t1_1
test12_2 = LM2.union t0_2 t1_2
test12_3 = LM1.union t1_1 t2_1
test12_4 = LM2.union t1_2 t2_2
test12_5 = LM1.unionWith (++) t0_1 t1_1
test12_6 = LM2.unionWith (++) t0_2 t1_2
test12_7 = LM1.unions [t0_1, t1_1, t2_1]
test12_8 = LM2.unions [t0_2, t1_2, t2_2]

joinPrev :: Int -> String -> State String String
joinPrev _ v = do
  prev <- get
  put v
  return $ v ++ ":" ++ prev

test13_1 = m where (m, _) = runState (LM1.traverseWithKey joinPrev test0_1) "0"
test13_2 = m where (m, _) = runState (LM2.traverseWithKey joinPrev test0_2) "0"

test14_1 = LM1.difference test6_1 (LM1.delete 1 $ LM1.delete 7 $ test0_1)
test14_2 = LM2.difference test6_2 (LM2.delete 1 $ LM2.delete 7 $ test0_2)
test15_1 = LM1.intersection test6_1 (LM1.delete 1 $ LM1.delete 7 $ test0_1)
test15_2 = LM2.intersection test6_2 (LM2.delete 1 $ LM2.delete 7 $ test0_2)
test16_1 = LM1.intersectionWith (\v1 v2 -> v1 ++ v2) test6_1 (LM1.delete 1 $ LM1.delete 7 $ test0_1)
test16_2 = LM2.intersectionWith (\v1 v2 -> v1 ++ v2) test6_2 (LM2.delete 1 $ LM2.delete 7 $ test0_2)

test17_1 = LM1.foldr (++) "" test6_1
test17_2 = LM2.foldr (++) "" test6_2
test18_1 = LM1.foldl' (++) "" test6_1
test18_2 = LM2.foldl' (++) "" test6_2

test19_1 = LM1.foldlWithKey' (\a k v -> a ++ (show k) ++ "=" ++ v ++ ",") "" test6_1
test19_2 = LM2.foldlWithKey' (\a k v -> a ++ (show k) ++ "=" ++ v ++ ",") "" test6_2
test20_1 = LM1.foldrWithKey (\k v a -> a ++ (show k) ++ "=" ++ v ++ ",") "" test6_1
test20_2 = LM2.foldrWithKey (\k v a -> a ++ (show k) ++ "=" ++ v ++ ",") "" test6_2

test21_1 = LM1.filter (\v -> v == "B" || v == "C") test0_1
test21_2 = LM2.filter (\v -> v == "B" || v == "C") test0_2
test22_1 = LM1.filterWithKey (\k v -> v == "B" || k == -6) test0_1
test22_2 = LM2.filterWithKey (\k v -> v == "B" || k == -6) test0_2

test23_1 = LM1.fromListWith (\v1 v2 -> v2 ++ v1) [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1 :: Int,"ZZZ")]
test23_2 = LM2.fromListWith (\v1 v2 -> v2 ++ v1) [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1 :: Int,"ZZZ")]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "test0" $ LM1.toList test0_1 @?= LM2.toList test0_2,
    testCase "test1" $ LM1.toList test1_1 @?= LM2.toList test1_2,
    testCase "test2" $ LM1.toList test2_1 @?= LM2.toList test2_2,
    testCase "test3" $ LM1.toList test3_1 @?= LM2.toList test3_2,
    testCase "test4" $ LM1.toList test4_1 @?= LM2.toList test4_2,
    testCase "test5" $ LM1.toList test5_1 @?= LM2.toList test5_2,
    testCase "test6" $ LM1.toList test6_1 @?= LM2.toList test6_2,
    testCase "test7_1" $ LM1.toList test7_1 @?= LM2.toList test7_2,
    testCase "test7_2" $ LM1.toList test7_1 @?= [(1,"AA"),(5,"BB"),(7,"CCC"),(-6,"D")],
    testCase "test8_1" $ LM1.toList test8_1 @?= LM2.toList test8_2,
    testCase "test8_2" $ LM1.toList test8_1 @?= [(1::Int, "D")],
    testCase "test9" $ LM1.toList test9_1 @?= LM2.toList test9_2,
    testCase "test10_1" $ LM1.toList test10_1 @?= LM2.toList test10_2,
    testCase "test10_2" $ LM1.toList test10_3 @?= LM2.toList test10_4,
    testCase "test11_1" $ LM1.toList test11_1 @?= LM2.toList test11_2,
    testCase "test11_2" $ LM1.toList test11_3 @?= LM2.toList test11_4,
    testCase "test12_1" $ LM1.toList test12_1 @?= LM2.toList test12_2,
    testCase "test12_1" $ LM1.toList test12_3 @?= LM2.toList test12_4,
    testCase "test12_2" $ LM1.toList test12_5 @?= LM2.toList test12_6,
    testCase "test12_3" $ LM1.toList test12_7 @?= LM2.toList test12_8,
    testCase "test13" $ LM1.toList test13_1 @?= LM2.toList test13_2,
    testCase "test14" $ LM1.toList test14_1 @?= LM2.toList test14_2,
    testCase "test15" $ LM1.toList test15_1 @?= LM2.toList test15_2,
    testCase "test16" $ LM1.toList test16_1 @?= LM2.toList test16_2,
    testCase "test17" $ test17_1 @?= test17_2,
    testCase "test18" $ test18_1 @?= test18_2,
    testCase "test19" $ test19_1 @?= test19_2,
    testCase "test20" $ test20_1 @?= test20_2,
    testCase "test21" $ LM1.toList test21_1 @?= LM2.toList test21_2,
    testCase "test22" $ LM1.toList test22_1 @?= LM2.toList test22_2,
    testCase "test23" $ LM1.toList test23_1 @?= LM2.toList test23_2
  ]

main :: IO ()
main = defaultMain unitTests
