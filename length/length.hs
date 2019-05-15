module Length where

length' :: Num p => [a] -> p
length' []     = 0
length' (x:xs) = 1 + length' xs
