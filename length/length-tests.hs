import Test.HUnit
import Length
import Prelude

test_one = TestCase $ assertEqual "length of [5, 1, 3, 2, 4] is 5" 5 (length [5, 1, 3, 2, 4])

tests_length =
  TestList [
    TestLabel "length of list one" test_one
  ]

main = do
  runTestTT tests_length
