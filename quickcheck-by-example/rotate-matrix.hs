import Data.List
import Test.QuickCheck

type Square = [[Integer]]

rotateCW :: Square -> Square
rotateCW = map reverse . transpose

rotateCCW :: Square -> Square
rotateCCW = reverse . transpose

rotateN :: (Square -> Square) -> Integer -> Square -> Square
rotateN _ 0 xs = xs
rotateN rotateFn n xs = rotateN rotateFn (n - 1) $ rotateFn xs

isValidMatrix :: Foldable t => [t a] -> Int -> Bool
isValidMatrix [] _ = False
isValidMatrix (x:[]) 0 = True
isValidMatrix (x:[]) n = length x == n
isValidMatrix (x:xs) 0 = isValidMatrix xs $ length x
isValidMatrix (x:xs) n = if (length x) /= n then False else isValidMatrix xs n

complete_rotation_clockwise matrix =
  if isValidMatrix matrix 0 == False then True else (rotateN rotateCW 4 matrix) == matrix
complete_rotation_counter_clockwise matrix =
  if isValidMatrix matrix 0 == False then True else (rotateN rotateCCW 4 matrix) == matrix
canceling_clockwise_rotation matrix =
  if isValidMatrix matrix 0 == False then True else (rotateCCW $ rotateCW matrix) == matrix
canceling_counter_clockwise_rotation matrix =
  if isValidMatrix matrix 0 == False then True else (rotateCCW $ rotateCW matrix) == matrix

nonEmptyString :: Gen [Integer]
nonEmptyString = listOf1 arbitrary

main :: IO()
main = do
  quickCheck $ forAll (listOf nonEmptyString) $ (canceling_clockwise_rotation :: [[Integer]] -> Bool)
  quickCheck $ forAll (listOf nonEmptyString) $ (canceling_counter_clockwise_rotation :: [[Integer]] -> Bool)
  quickCheck $ forAll (listOf nonEmptyString) $ (complete_rotation_clockwise :: [[Integer]] -> Bool)
  quickCheck $ forAll (listOf nonEmptyString) $ (complete_rotation_counter_clockwise :: [[Integer]] -> Bool)
