module SetTests where

import Data.LinkedHashSet

test0 = fromList [1 :: Int, 5, 7, -6]

y1 = insert 3 test0
y2 = insert (-10) $ insert 3 test0
y3 = delete 5 $ insert (-10) $ insert 3 test0
y4 = delete 3 $ delete 5 $ insert (-10) $ insert 3 test0
y7 = delete 3 $ insert 3 $ insert 3 test0

test1 = fromList [1 :: Int, 5, 7, -6, 1]
test2 = fromList [1 :: Int, 5, 7, -6, 1, 5, 7, 7]
test3 = fromList [1 :: Int, 1, 1, 2]

test4 = fromList ([] :: [Int])

