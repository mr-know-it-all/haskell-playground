import Data.Char
import Data.List

-- rule 110
getNextCellState :: Int -> Int -> Int -> Int
getNextCellState 1 1 1 = 0
getNextCellState 1 0 0 = 0
getNextCellState 0 0 0 = 0
getNextCellState _ _ _ = 1

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

computeNextGen :: [Int] -> [Int]
computeNextGen n = mapInd (\x -> \i -> getNextCellState (left n i) (elem n i) (right n i)) n
                                       where
                                        left n i = if i - 1 < 0 then 0 else n!!(i - 1)
                                        elem n i = n!!i
                                        right n i = if (length n) - 1 < i + 1 then 0 else n!!(i + 1)

prepareForPrint n = intercalate "" $ map (\x -> if x == 0 then " " else ".") n

rule110 :: [Int] -> IO()
rule110 n =
  do
   print $ prepareForPrint n
   rule110 $ computeNextGen n

main :: IO()
main = do
  rule110 $ take 200 [1, 1..]
