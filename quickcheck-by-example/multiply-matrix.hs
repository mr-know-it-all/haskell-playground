import Data.List
import Test.QuickCheck

mmult :: Num a => [[a]] -> [[a]] -> [[a]]
mmult a b =
  if isValidMatrix a 0 && isValidMatrix b 0
  then [[ sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a ]
  else []

isValidMatrix :: Foldable t => [t a] -> Int -> Bool
isValidMatrix [] _ = False
isValidMatrix (xs:xss) n = if (length xs) /= n then False else isValidMatrix xss n

multiply_associative m1 m2 m3 =
  mmult (mmult m1 m2) m3 == mmult m1 (mmult m2 m3)

-- zero is []
multiply_with_zero m = mmult [] m == mmult m []

nonEmptyString :: Gen [Integer]
nonEmptyString = listOf1 arbitrary

main :: IO()
main = do
  quickCheck $
    forAll (listOf nonEmptyString) $ \m1 ->
    forAll (listOf nonEmptyString) $ \m2 ->
    forAll (listOf nonEmptyString) $ \m3 ->
      multiply_associative m1 m2 m3
  quickCheck $
    forAll (listOf nonEmptyString) $ \m ->
      multiply_with_zero m
