-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigit :: (Integer, [Integer]) -> Bool
testToRevDigit (n, ns) = toRevDigits n == ns

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigitsTest" testToRevDigit
             [(1, [1]), (12, [2,1]), (789, [9,8,7])]]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (xs, ys) = doubleEveryOther xs == ys

makeDoubleEveryOtherTest :: [Integer] -> [Integer] -> String -> Test
makeDoubleEveryOtherTest xs ys name = Test ("Double every other test: " ++ name) testDoubleEveryOther [(xs, ys)]

ex3Tests :: [Test]
ex3Tests = [ makeDoubleEveryOtherTest [] [] "empty"
           , makeDoubleEveryOtherTest [4, 5] [4, 10] "4 and 5"
           , makeDoubleEveryOtherTest [7, 8, 9] [7, 16, 9] "7, 8, 9"
           ]

-- Exercise 4 -----------------------------------------

makeSumDigitsTest :: [Integer] -> Integer -> Test
makeSumDigitsTest ints sum = testF1 "sum digits test" sumDigits [(ints, sum)]

ex4Tests :: [Test]
ex4Tests = [ makeSumDigitsTest [1, 2, 3, 4] 10
           , makeSumDigitsTest [4, 5, 6] 15
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  ]
