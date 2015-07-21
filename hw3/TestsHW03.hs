{-# OPTIONS_GHC -Wall #-}
module TestsHW03 where

import HW03

data Test a = Test String (a -> Bool) a

state :: State
state = let s1 = extend empty "A" 1
            s2 = extend s1 "B" 2
            s3 = extend s2 "C" 3
            s4 = extend s3 "D" 4
        in extend s4 "E" 5

makeTest :: (Eq i, Eq o) => String -> (i -> o) -> [i] -> [o] -> Test [i]
makeTest name f inputs expectedOutputs = Test name testFunction inputs
  where testFunction list = expectedOutputs == map f list

extendTest :: Test [String]
extendTest = makeTest "test of extend function" state ["A","B","C","D","E"] [1,2,3,4,5]

runTest :: Test t -> Maybe String
runTest (Test name p a)
  | p a       = Nothing
  | otherwise = Just ("Failed: " ++ name)

