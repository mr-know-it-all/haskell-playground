module Quicksort where

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort small ++ mid ++ quicksort large
  where
    small = [y | y <- xs, y < x]
    mid   = [y | y <- xs, y == x] ++ [x]
    large = [y | y <- xs, y > x]
