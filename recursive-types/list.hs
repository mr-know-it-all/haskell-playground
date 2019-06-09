module List where

data List a = Empty | Cons a (List a) deriving (Show, Eq)

length' :: Num n => List a -> n
length' Empty = 0
length' (Cons _ xs) = 1 + (length' xs)

map' :: (a -> b) -> List a -> List b
map' _ Empty = Empty
map' fn (Cons x xs) = Cons (fn x) (map' fn xs)
