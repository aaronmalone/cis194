{-# OPTIONS_GHC -Wall #-}
module Tests where

import HW04

test :: String -> Bool -> String
test name result = (if result then "Passed" else "Failed") ++ ": " ++ name

allTests :: [String]
allTests = 
   [ test "Basic Poly test 1" (P [1, 2, 3] == P [1, 2, 3])
   , test "Poly test different lengths" (P [4.0, 5.0, 0.0] == P [4.0, 5.0])
   , test "Test show, example 1" (show (P [1, 0, 0, 2]) == "2x^3 + 1")
   , test "Test show, example 2" (show (P [0, -1, 2]) == "2x^2 + -x")
   ]

showTestResults :: IO ()
showTestResults = putStrLn (unlines allTests)