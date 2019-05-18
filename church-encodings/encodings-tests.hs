import Test.HUnit
import Encodings
import Prelude hiding (succ, pred, and, or, not, exp, head, tail)

test_true = TestCase $ assertBool "true == True" (unchurch_bool true)
test_true_num = TestCase $ assertEqual "true 2.5 3.2 == 2.5" 2.5 (true 2.5 3.2)
test_false = TestCase $ assertEqual "false == False" False (unchurch_bool false)
test_false_num = TestCase $ assertEqual "false 5 6 == 6" 6 (false 5 6)

tests_bools =
  TestList [
    TestLabel "test true" test_true,
    TestLabel "test true number" test_true_num,
    TestLabel "test false" test_false,
    TestLabel "test false number" test_false_num
  ]

test_true_and_true = TestCase $ assertBool "true and true == True" (unchurch_bool $ and true true)
test_true_and_false = TestCase $ assertEqual "true and false == False" False (unchurch_bool $ and true false)
test_false_and_true = TestCase $ assertEqual "false and true == False" False (unchurch_bool $ and false true)
test_false_and_false = TestCase $ assertEqual "false and false == False" False (unchurch_bool $ and false false)

test_true_or_true = TestCase $ assertBool "true or true == True" (unchurch_bool $ or true true)
test_true_or_false = TestCase $ assertBool "true or false == True" (unchurch_bool $ or true false)
test_false_or_true = TestCase $ assertBool "false or true == True" (unchurch_bool $ or false true)
test_false_or_false = TestCase $ assertEqual "false or false == False" False (unchurch_bool $ or false false)

test_true_xor_true = TestCase $ assertEqual "true xor true == False" False (unchurch_bool $ xor true true)
test_true_xor_false = TestCase $ assertBool "true xor false == True" (unchurch_bool $ xor true false)
test_false_xor_true = TestCase $ assertBool "false xor true == True" (unchurch_bool $ xor false true)
test_false_xor_false = TestCase $ assertEqual "false xor false == False" False (unchurch_bool $ xor false false)

test_not_true = TestCase $ assertEqual "not true == False" False (unchurch_bool $ not true)
test_not_false = TestCase $ assertBool "not false == True" (unchurch_bool $ not false)
test_not_false_or_true = TestCase $ assertEqual "not false or true == False" False (unchurch_bool $ not $ or false true)
test_not_true_and_false = TestCase $ assertBool "not true and false == True" (unchurch_bool $ not $ and true false)
test_not_true_xor_true = TestCase $ assertBool "not true xor true == True" (unchurch_bool $ not $ xor true true)

test_ifelse_true = TestCase $ assertEqual "ifelse true two three == 2" 2 (unchurch_num $ ifelse true two three)
test_ifelse_false = TestCase $ assertEqual "ifelse false two three == 3" 3 (unchurch_num $ ifelse false two three)

tests_conditionals =
  TestList [
    TestLabel "test true and true" test_true_and_true,
    TestLabel "test true and false" test_true_and_false,
    TestLabel "test false and true" test_false_and_true,
    TestLabel "test false and false" test_false_and_false,

    TestLabel "test true or true" test_true_or_true,
    TestLabel "test true or false" test_true_or_false,
    TestLabel "test false or true" test_false_or_true,
    TestLabel "test false or false" test_false_or_false,

    TestLabel "test true xor true" test_true_xor_true,
    TestLabel "test true xor false" test_true_xor_false,
    TestLabel "test false xor true" test_false_xor_true,
    TestLabel "test false xor false" test_false_xor_false,

    TestLabel "test not true" test_not_true,
    TestLabel "test not false" test_not_false,
    TestLabel "test not false or true" test_not_false_or_true,
    TestLabel "test not true or false" test_not_true_and_false,
    TestLabel "test not true xor true" test_not_true_xor_true,

    TestLabel "test ifelse true" test_ifelse_true,
    TestLabel "test ifelse false" test_ifelse_false
  ]

test_zero = TestCase $ assertEqual "numeral zero == 0" 0 (unchurch_num zero)
test_one = TestCase $ assertEqual "numeral one == 1" 1 (unchurch_num one)
test_two = TestCase $ assertEqual "numeral two == 2" 2 (unchurch_num two)
test_three = TestCase $ assertEqual "numeral three == 3" 3 (unchurch_num three)
test_num_12 = TestCase $ assertEqual "numeral three == 12" 12 (unchurch_num $ num 12)
test_num_17 = TestCase $ assertEqual "numeral three == 17" 17 (unchurch_num $ num 17)
test_num_123779 = TestCase $ assertEqual "numeral 123779 == 123779" 123779 (unchurch_num $ num 123779)
test_if_zero = TestCase $ assertBool "is_zero zero == True" (unchurch_bool $ is_zero zero)

tests_numerals =
  TestList [
    TestLabel "test zero" test_zero,
    TestLabel "test one" test_one,
    TestLabel "test two" test_two,
    TestLabel "test three" test_three,
    TestLabel "test numeral 12" test_num_12,
    TestLabel "test numeral 17" test_num_17,
    TestLabel "test numeral 123779" test_num_123779,
    TestLabel "test if is zero" test_if_zero
  ]

test_succ_one = TestCase $ assertEqual "succ of 1 is 2" 2 (unchurch_num $ succ $ num 1)
test_succ_two = TestCase $ assertEqual "succ of 1 is 2" 2123 (unchurch_num $ succ $ num 2122)
test_pred_one = TestCase $ assertEqual "pred of 1 is 0" 0 (unchurch_num $ pred $ num 1)
test_pred_two = TestCase $ assertEqual "pred of 2122 is 2121" 2121 (unchurch_num $ pred $ num 2122)
test_add_one = TestCase $ assertEqual "add 1 and 2 is 3" 3 (unchurch_num $ add (num 1) (num 2))
test_add_two = TestCase $ assertEqual "add 1123 and 12312 is 13435" 13435 (unchurch_num $ add (num 1123) (num 12312))
test_mult_one = TestCase $ assertEqual "mult 1 and 2 is 2" 2 (unchurch_num $ mult (num 1) (num 2))
test_mult_two = TestCase $ assertEqual "mult 1123 and 11234 is 1261129" 1261129 (unchurch_num $ mult (num 1123) (num 1123))
test_exp_one = TestCase $ assertEqual "2 pow 3 is 8" 8 (unchurch_num $ exp (num 2) (num 3))
test_exp_two = TestCase $ assertEqual "21 pow 4 is 194481" 194481 (unchurch_num $ exp (num 21) (num 4))

test_number_operations =
  TestList [
    TestLabel "test succ one" test_succ_one,
    TestLabel "test succ two" test_succ_two,
    TestLabel "test pred one" test_pred_two,
    TestLabel "test pred two" test_pred_two,
    TestLabel "test add one" test_add_one,
    TestLabel "test add two" test_add_two,
    TestLabel "test mult one" test_mult_one,
    TestLabel "test mult two" test_mult_two,
    TestLabel "test exp one" test_exp_one,
    TestLabel "test exp two" test_exp_two
  ]

fac :: (Int -> Int) -> Int -> Int
fac m n = if n == 0 then 1 else n * m (n - 1)

test_y_one = TestCase $ assertEqual "factorial of 5 is" 120 $ y fac 5
test_y_two = TestCase $ assertEqual "factorial of 5 is" 5040 $ y fac 7

tests_y_combinator =
  TestList [
    TestLabel "test y combinator one" test_y_one,
    TestLabel "test y combinator two" test_y_two
  ]

main = do
  runTestTT tests_bools
  runTestTT tests_conditionals
  runTestTT tests_numerals
  runTestTT tests_y_combinator
  runTestTT test_number_operations
