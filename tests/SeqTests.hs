module SeqTests where

import Data.LinkedHashMap.Seq

test0 = fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D")]
test1 = fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1 :: Int,"AAAAA")]
test2 = fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1,"AA"), (5,"BB"), (7,"CC"),(7,"CCC")]
test3 = fromList [(1 :: Int,"A"), (1, "B"), (1, "C"), (1, "D")]

y0@(LinkedHashMap mm0 _s0 _n0) = test0
y1@(LinkedHashMap mm1 s1 n1) = insert 3 "ZZ" test0
y2@(LinkedHashMap mm2 s2 n2) = insert (-10) "AA" $ insert 3 "ZZ" test0
y3@(LinkedHashMap mm3 s3 n3) = delete 5 $ insert (-10) "AA" $ insert 3 "ZZ" test0
y4@(LinkedHashMap mm4 s4 n4) = delete 3 $ delete 5 $ insert (-10) "AA" $ insert 3 "ZZ" test0
y5@(LinkedHashMap mm5 s5 n5) = insert 3 "ZZ" test0
y6@(LinkedHashMap mm6 s6 n6) = insert 3 "AA" y5
y7@(LinkedHashMap mm7 s7 n7) = delete 3 $ insert 3 "AA" $ insert 3 "ZZ" test0

y8@(LinkedHashMap mm8 s8 n8) = delete 5 $ insert (-10) "AA" $ insert 3 "ZZ" test0
y9@(LinkedHashMap mm9 s9 n9) = delete 3 y8

z0@(LinkedHashMap zm0 zs0 zn0) = test1
z1@(LinkedHashMap zm1 zs1 zn1) = test2
z2@(LinkedHashMap zm2 zs2 zn2) = test3
z3@(LinkedHashMap zm3 zs3 zn3) = delete 7 $ test0
z4@(LinkedHashMap zm4 zs4 zn4) = delete 1 $ z3
z5@(LinkedHashMap zm5 zs5 zn5) = delete 5 $ z4

test4 = fromList ([] :: [(Int, String)])

y3'@(LinkedHashMap mm3' s3' n3') = pack y3

