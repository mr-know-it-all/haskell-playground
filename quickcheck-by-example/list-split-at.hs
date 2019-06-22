-- example from schoolofhaskell.com

import Test.QuickCheck
import Data.List (intersperse)

--(?) :: Bool -> (t, t) -> t
a ? (b, c) = if a then b else c

split :: Char -> String -> [String]
split c xs = splitList
  where part = takeWhile (/=c) xs
        rest = dropWhile (/=c) xs
        splitList =  part : null rest ? ([], (split c (tail rest)))

unsplit :: Char -> [String] -> String
unsplit c = concat . intersperse (c : [])

reversable_property xs = forAll (elements xs) (\c -> unsplit c (split c xs) == xs)

main = quickCheck reversable_property
