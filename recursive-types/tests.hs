import Test.HUnit
import List
import Prelude

test_length =
  TestCase $ assertEqual
    "expecting length of 2"
    2
    (length' (Cons 1 (Cons 2 Empty)))
test_map =
  TestCase $ assertEqual
    "expecting map to work"
    (Cons 2 (Cons 3 Empty))
    $ map' (\x -> x + 1) (Cons 1 (Cons 2 Empty))
test_filter =
  TestCase $ assertEqual
    "expecting filter to work"
    (Cons 2 Empty)
    $ filter' (\x -> x > 1) (Cons 1 (Cons 2 Empty))
test_reduce_sum =
  TestCase $ assertEqual
    "expecting reduce to work for sum"
    42
    $ reduce' 0 (\acc v -> acc + v) (Cons 40 (Cons 2 Empty))
test_reduce_product =
  TestCase $ assertEqual
    "expecting reduce to work for product"
    160
    $ reduce' 1 (\acc v -> acc * v) (Cons 40 (Cons 2 (Cons 2 Empty)))
test_reduce_concat =
  TestCase $ assertEqual
    "expecting reduce to work for concat"
    "abc"
    $ reduce' "" (\acc v -> acc ++ v) (Cons "a" (Cons "b" (Cons "c" Empty)))

tests_custom_list =
  TestList [
    TestLabel "test list length" test_length,
    TestLabel "test list map" test_map,
    TestLabel "test list filter" test_filter,
    TestLabel "test list reduce with sum" test_reduce_sum,
    TestLabel "test list reduce with product" test_reduce_product,
    TestLabel "test list reduce with concat" test_reduce_concat
  ]

main = do
  runTestTT tests_custom_list
