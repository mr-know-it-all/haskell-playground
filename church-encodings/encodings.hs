module Encodings where
import Prelude hiding (succ, pred, and, or, not, exp, head, tail)

-- λt.λf.t
true :: a -> a -> a
true = \t -> \f -> t

-- λt.λf.f
false :: a -> a -> a
false = \t -> \f -> f

-- (λa.λb.λc.cab) True False
unchurch_bool :: (Bool -> Bool -> a) -> a
unchurch_bool = (\a -> \b -> \c -> c a b) True False

-- λa.λb.aba
and :: (a1 -> (a -> a -> a) -> a0) -> a1 -> a0
and = \a -> \b -> a b false

-- λa.λb.aab
or :: ((a -> a -> a) -> a1 -> a0) -> a1 -> a0
or = \a -> \b -> a true b

-- λp.λa.λb.pba
not = \p -> \a -> \b -> p b a

-- λa.λb.a(notb)b
xor = \a -> \b -> a (not b) b

-- λp.λa.λb.pab
ifelse :: (a -> a -> a) -> a -> a -> a
ifelse = \p -> \a -> \b -> p a b

-- λf.λx.x
zero :: (a -> a) -> a -> a
zero = \f -> \x -> x

-- λf.λx.f x
one :: (a -> a) -> a -> a
one = \f -> \x -> f x

-- λf.λx.f (f x)
two :: (a -> a) -> a -> a
two = \f -> \x -> f (f x)

-- λf.λx.f (f (f x))
three :: (a -> a) -> a -> a
three = \f -> \x -> f (f (f x))

-- num 0 = λf.λx.x
-- num n = λf.λx.f (num (n-1) f x)
num :: Integer -> (a -> a) -> a -> a;
num 0 = \f -> \x -> x;
num n = \f -> \x -> f (num (n-1) f x)

-- λn.n (λx.false) true
is_zero :: ((a2 -> a -> a -> a) -> (a1 -> a1 -> a1) -> a0) -> a0
is_zero = \n -> n (\x -> false) true

-- λa.a (λb.b+1) (0)
unchurch_num :: ((Integer -> Integer) -> Integer -> a) -> a
unchurch_num = \a -> a (\b -> b + 1) (0)
