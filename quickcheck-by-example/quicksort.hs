import Test.QuickCheck
import Data.List

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort small ++ [x] ++ quicksort large
  where
    small = filter (<x) xs
    large = filter (>=x) xs

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted xs

idempotent_property xs = quicksort (quicksort xs) == quicksort xs
sorted_pproperty xs = isSorted (quicksort xs)

main = do
  quickCheck (idempotent_property :: [Integer] -> Bool)
  quickCheck (sorted_pproperty :: [Integer] -> Bool)
