module Main where

import Test.Tasty
import Test.Tasty.HUnit

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

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "test0" $ LM1.toList test0_1 @?= LM2.toList test0_2,
    testCase "test1" $ LM1.toList test1_1 @?= LM2.toList test1_2,
    testCase "test2" $ LM1.toList test2_1 @?= LM2.toList test2_2,
    testCase "test3" $ LM1.toList test3_1 @?= LM2.toList test3_2,
    testCase "test4" $ LM1.toList test4_1 @?= LM2.toList test4_2,
    testCase "test5" $ LM1.toList test5_1 @?= LM2.toList test5_2,
    testCase "test6" $ LM1.toList test6_1 @?= LM2.toList test6_2,
    testCase "test7" $ LM1.toList test7_1 @?= LM2.toList test7_2,
    testCase "test7_2" $ LM1.toList test7_1 @?= [(1,"AA"),(5,"BB"),(7,"CCC"),(-6,"D")],
    testCase "test8" $ LM1.toList test8_1 @?= LM2.toList test8_2,
    testCase "test8_2" $ LM1.toList test8_1 @?= [(1::Int, "D")],
    testCase "test9" $ LM1.toList test9_1 @?= LM2.toList test9_2
  ]

main :: IO ()
main = defaultMain unitTests
