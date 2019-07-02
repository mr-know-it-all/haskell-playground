import Data.List
import Test.QuickCheck

data Matrix = Square [[Integer]] | SingleValue Integer deriving (Eq, Show)

mmult :: Matrix -> Matrix -> Matrix
mmult (SingleValue n) (Square xss) = Square [[ x * n | x <- xs] | xs <- xss ]
mmult (Square xss) (SingleValue n) = Square [[ x * n | x <- xs] | xs <- xss ]
mmult (Square a) (Square b)        = Square [[ sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a ]

isValidMatrix :: Foldable t => [t a] -> Bool
isValidMatrix xss = (length $ nub [ length xs | xs <- xss ]) == 0 -- TODO: write more performant code

multiply_associative m1 m2 m3 =
  mmult (mmult (Square m1) (Square m2)) (Square m3) == mmult (Square m1) (mmult (Square m2) (Square m3))
multiply_with_zero m =
  mmult (SingleValue 0) (Square m) == mmult (Square m) (SingleValue 0)
multiply_with_single_value m x =
  mmult (SingleValue x) (Square m) == mmult (Square m) (SingleValue x)

nonEmptyString :: Gen [Integer]
nonEmptyString = listOf1 arbitrary

matrixGen :: Gen [[Integer]]
matrixGen = (listOf nonEmptyString) `suchThat` isValidMatrix

integerGen :: Gen Integer
integerGen = abs `fmap` (arbitrary :: Gen Integer)

main :: IO()
main = do
  quickCheck $
    forAll matrixGen $ \m1 ->
    forAll matrixGen $ \m2 ->
    forAll matrixGen $ \m3 ->
      multiply_associative m1 m2 m3
  quickCheck $
    forAll matrixGen $ \m ->
    forAll integerGen $ \x ->
      multiply_with_single_value m x
  quickCheck $
    forAll matrixGen $ \m ->
      multiply_with_zero m
