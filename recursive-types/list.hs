module List where

data List a = Empty | Cons a (List a) deriving (Show, Eq)

length' :: Num n => List a -> n
length' Empty = 0
length' (Cons _ xs) = 1 + (length' xs)

map' :: (a -> b) -> List a -> List b
map' _ Empty = Empty
map' fn (Cons x xs) = Cons (fn x) (map' fn xs)

filter' :: (a -> Bool) -> List a -> List a
filter' _ Empty = Empty
filter' pred (Cons x xs)
  | pred x = Cons x (filter' pred xs)
  | otherwise = filter' pred xs

reduce' :: t1 -> (t2 -> t1 -> t1) -> List t2 -> t1
reduce' init _ Empty = init
reduce' init fn (Cons x xs) = fn x (reduce' init fn xs)
