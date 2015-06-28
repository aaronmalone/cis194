{-# OPTIONS_GHC -Wall #-}

module Lecture where

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

canItBeNegative :: Integer
canItBeNegative = (-1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0  = n `div` 2
  | otherwise       = 3*n + 1

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

isEven' :: Int -> Bool
isEven' n = mod n 2 == 0

isEven'' :: Int -> Bool
isEven'' = (== 0) . flip mod 2

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)
