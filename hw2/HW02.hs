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

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move guess _ _) code = getMove code guess == m

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
