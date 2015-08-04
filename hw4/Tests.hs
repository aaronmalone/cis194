module Tests where

import HW04

test :: String -> Bool -> String
test name result = (if result then "Passed" else "Failed") ++ ": " ++ name

allTests :: [String]
allTests = 
   [ test "Basic Poly test 1" (P [1, 2, 3] == (3*x^2 + 2*x + 1))
   , test "Poly test different lengths" (P [4.0, 5.0, 0.0] == (P [4.0, 5.0]))
   , test "Test show, example 1" (show (2*x^3+1) == "2x^3 + 1")
   , test "Test show, example 2" (show (2*x^2 - x) == "2x^2 + -x")
   , test "Test applyP, example 1" (applyP (x^2 + 2*x + 1) 1 == 4)
   , test "Test applyP, example 2" (applyP (x^2 + 2*x + 1) 2 == 9)
   ]

showTestResults :: IO ()
showTestResults = putStrLn (unlines allTests)