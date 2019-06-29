import Data.List
import Test.QuickCheck

data Matrix = Square [[Integer]] | SingleValue Integer deriving (Eq, Show)

mmult :: Matrix -> Matrix -> Matrix
mmult (SingleValue _) _ = SingleValue 0
mmult _ (SingleValue _) = SingleValue 0
mmult (Square a) (Square b) =
  if isValidMatrix a 0 && isValidMatrix b 0 -- remove and generate valid values
  then Square [[ sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a ]
  else SingleValue 0

isValidMatrix :: Foldable t => [t a] -> Int -> Bool
isValidMatrix [] _ = False
isValidMatrix (xs:xss) n = if (length xs) /= n then False else isValidMatrix xss n

multiply_associative m1 m2 m3 =
  mmult (mmult (Square m1) (Square m2)) (Square m3) == mmult (Square m1) (mmult (Square m2) (Square m3))

multiply_with_zero m = mmult (SingleValue 0) (Square m) == mmult (Square m) (SingleValue 0)

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
