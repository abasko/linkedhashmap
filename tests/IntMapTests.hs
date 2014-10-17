module IntMapTests where

import Data.LinkedHashMap.IntMap

test0 = fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "C")]

y0@(LinkedHashMap mm0 s0 maxn0) = test0
y1@(LinkedHashMap mm1 s1 maxn1) = insert 3 "ZZ" test0
y2@(LinkedHashMap mm2 s2 maxn2) = insert (-10) "AA" $ insert 3 "ZZ" test0
y3@(LinkedHashMap mm3 s3 maxn3) = delete 5 $ insert (-10) "AA" $ insert 3 "ZZ" test0
y4@(LinkedHashMap mm4 s4 maxn4) = delete 3 $ delete 5 $ insert (-10) "AA" $ insert 3 "ZZ" test0
y4_1@(LinkedHashMap mm4_1 s4_1 maxn4_1) = insert 11 "QQ" $ y4
y5@(LinkedHashMap mm5 s5 maxn5) = insert 3 "ZZ" test0
y6@(LinkedHashMap mm6 s6 maxn6) = insert 3 "AA" y5
y7@(LinkedHashMap mm7 s7 maxn7) = delete 3 $ insert 3 "AA" $ insert 3 "ZZ" test0

test1 = fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1 :: Int,"AAAAA")]
test2 = fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1,"AA"), (5,"BB"), (7,"CC"),(7,"CCC")]
test3 = fromList [(1 :: Int,"A"), (1, "B"), (1, "C"), (2, "D")]

z0@(LinkedHashMap zm0 zs0 zmaxn0) = test1
z1@(LinkedHashMap zm1 zs1 zmaxn1) = test2
z2@(LinkedHashMap zm2 zs2 zmaxn2) = test3

test4 = fromList ([] :: [(Int, String)])

