{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys)
  | x == y    = 1 + exactMatches xs ys
  | otherwise = exactMatches xs ys

-- Exercise 2 -----------------------------------------

countColors :: Code -> [Int]
countColors code = map countColor colors
  where countColor :: Peg -> Int
        countColor peg = length $ filter (== peg) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ map (uncurry min) (countColors xs `zip` countColors ys)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonExact
  where exact    = exactMatches secret guess
        nonExact = (matches secret guess) - exact

-- Exercise 4 -----------------------------------------

--  a Code is consistent with a Move if the Code could have been the secret that generated that move
isConsistent :: Move -> Code -> Bool
isConsistent m@(Move guess _ _) code = getMove code guess == m

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (:[]) colors
allCodes n = concatMap codesWithOneMorePeg (allCodes (n - 1))
  where codesWithOneMorePeg :: Code -> [Code]
        codesWithOneMorePeg code = map (:code) colors


-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = solve' secret (allCodes (length secret)) []

-- get a move from the first guess and the secret
-- if move is exact match, return as list
-- if move is not exact match, filter all consistent remaining move
solve' :: Code -> [Code] -> [Move] -> [Move]
solve' s (next:remaining) attempted
  | s == next = reverse newAttemptedList
  | otherwise = solve' s consistent newAttemptedList
  where newAttemptedList = nextMove : attempted
        consistent       = filter (isConsistent nextMove) remaining
        nextMove         = getMove s next
solve' _ [] _ = error "Exhausted all options!"

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
