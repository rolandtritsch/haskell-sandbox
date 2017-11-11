-- QSort.hs
module QSort where

import Test.QuickCheck

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs where
  lhs = filter (< x) xs
  rhs = filter (>= x) xs

prop_max :: [Int] -> Property
prop_max xs = not (null xs) ==> last (qsort xs) == maximum xs

main :: IO ()
main = quickCheck prop_max
