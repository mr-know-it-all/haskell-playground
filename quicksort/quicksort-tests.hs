import Test.HUnit
import Quicksort
import Prelude

test_one = TestCase $ assertEqual "[5, 1, 3, 2, 4] to [1, 2, 3, 4, 5]" [1, 2, 3, 4, 5] (quicksort [5, 1, 3, 2, 4])
test_two = TestCase $ assertEqual "[5, -11, 3, -2, 4] to [-11, -2, 3, 4, 5]" [-11, -2, 3, 4, 5] (quicksort [5, -11, 3, -2, 4])
test_three = TestCase $ assertEqual "[200, 0, 100, 0, 0] to [0, 0, 0, 100, 200]" [0, 0, 0, 100, 200] (quicksort [200, 0, 100, 0, 0])

tests_quicksort =
  TestList [
    TestLabel "sort list one" test_one,
    TestLabel "sort list two" test_two,
    TestLabel "sort list three" test_three
  ]

main = do
  runTestTT tests_quicksort
