module SeqTests where

import Prelude hiding (lookup, filter)
import Control.Monad.State
import Data.Hashable
import Data.LinkedHashMap.Seq
import qualified Data.LinkedHashMap.Seq as LHM
import qualified Data.HashMap.Strict as M

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

z6 = insertWith (++) 5 "_ZZZ" test0
z7 = insertWith (++) 11 "_ZZZ" test0

z8 = adjust (\v0 -> v0 ++ "_adjusted") 5 test0
z9 = adjust (\v0 -> v0 ++ "_adjusted") 123 test0

t0 = fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D")]
t1 = fromList [(2 :: Int,"2A"), (3, "2B"), (7, "2C"), (-6, "2D")]
t2 = fromList [(0 :: Int,"3A"), (5, "3B"), (17, "3C"), (-6, "3D")]

u1 = union t0 t1
u2 = union t1 t2
u3 = unionWith (++) t0 t1
u4 = unions [t0, t1, t2]

m1 = mapWithKey (\k v1 -> v1 ++ show k) t0

hm0 = M.fromList [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D")]

printItem :: Show a => a -> String -> IO String
printItem k v = do
  putStrLn $ (show k) ++ "->" ++ v
  return $ v ++ "_processed"

joinPrev :: Int -> String -> State String String
joinPrev _ v = do
  prev <- get
  put v
  return $ v ++ ":" ++ prev

a0 = traverseWithKey printItem test0
a1 = m where (m, _) = runState (traverseWithKey joinPrev test0) "0"

d0 = difference test1 (delete 1 $ delete 7 $ test0)
i0 = intersection test1 (delete 1 $ delete 7 $ test0)
ik0 = intersectionWith (\v1 v2 -> v1 ++ v2) test1 (delete 1 $ delete 7 $ test0)

f0 = LHM.foldr (++) "" test1
f1 = LHM.foldr (++) "" y3
f3 = LHM.foldl' (++) "" test1
f4 = LHM.foldl' (++) "" y3

fk0 = foldlWithKey' (\a k v -> a ++ (show k) ++ "=" ++ v ++ ",") "" test1
fk1 = foldrWithKey (\k v a -> a ++ (show k) ++ "=" ++ v ++ ",") "" test1

ff0 = filter (\v -> v == "B" || v == "C") test0
ff1 = filterWithKey (\k v -> v == "B" || k == -6) test0

fff0 = fromListWith (\v1 v2 -> v2 ++ v1) [(1 :: Int,"A"), (5, "B"), (7, "C"), (-6, "D"), (1 :: Int,"ZZZ")]
