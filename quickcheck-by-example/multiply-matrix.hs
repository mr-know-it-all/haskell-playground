import Data.List
import Test.QuickCheck

data Matrix = Square [[Integer]] | SingleValue Integer deriving (Eq, Show)

mmult :: Matrix -> Matrix -> Matrix -- TODO: refactor for cleaner code
mmult (SingleValue n) (Square xss) =
  if isValidMatrix xss 0 -- remove and generate valid values
  then Square [[x * n | x <- xs] | xs <- xss]
  else SingleValue 0
mmult (Square xss) (SingleValue n) =
  if isValidMatrix xss 0 -- remove and generate valid values
  then Square [[x * n | x <- xs] | xs <- xss]
  else SingleValue 0
mmult (Square a) (Square b) =
  if isValidMatrix a 0 && isValidMatrix b 0 -- remove and generate valid values
  then Square [[ sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a ]
  else SingleValue 0

isValidMatrix :: Foldable t => [t a] -> Int -> Bool
isValidMatrix [] _ = False
isValidMatrix (xs:xss) n = if (length xs) /= n then False else isValidMatrix xss n

multiply_associative m1 m2 m3 =
  mmult (mmult (Square m1) (Square m2)) (Square m3) == mmult (Square m1) (mmult (Square m2) (Square m3))
multiply_with_zero m =
  mmult (SingleValue 0) (Square m) == mmult (Square m) (SingleValue 0)
multiply_with_single_value m x =
  mmult (SingleValue x) (Square m) == mmult (Square m) (SingleValue x)

nonEmptyString :: Gen [Integer]
nonEmptyString = listOf1 arbitrary

integerGen :: Gen Integer
integerGen = abs `fmap` (arbitrary :: Gen Integer)

main :: IO()
main = do
  quickCheck $
    forAll (listOf nonEmptyString) $ \m1 ->
    forAll (listOf nonEmptyString) $ \m2 ->
    forAll (listOf nonEmptyString) $ \m3 ->
      multiply_associative m1 m2 m3
  quickCheck $
    forAll (listOf nonEmptyString) $ \m ->
    forAll integerGen $ \x ->
      multiply_with_single_value m x
  quickCheck $
    forAll (listOf nonEmptyString) $ \m ->
      multiply_with_zero m
