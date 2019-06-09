import Test.HUnit
import List
import Prelude

test_one = TestCase $ assertEqual "expecting length of 2" 2 (length' (Cons 1 (Cons 2 Empty)))
test_two = TestCase $ assertEqual "expecting map to work" (Cons 2 (Cons 3 Empty)) $ map' (\x -> x + 1) (Cons 1 (Cons 2 Empty))

tests_custom_list =
  TestList [
    TestLabel "test list length" test_one,
    TestLabel "test list map" test_two
  ]

main = do
  runTestTT tests_custom_list
