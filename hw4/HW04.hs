{-# OPTIONS_GHC -Wall #-}
module HW04 
   (
     Poly(P)
   ) where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P y) (P z) = equalsOrZero y z
      where equalsOrZero []     []     = True
            equalsOrZero (k:ks) (j:js) = k == j && equalsOrZero ks js
            equalsOrZero []     (j:js) = j == 0 && equalsOrZero [] js
            equalsOrZero (k:ks) []     = k == 0 && equalsOrZero ks []
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show poly | poly == P [] = "0"
    show (P coefs)           =
       let zipWithDeg   = zip coefs [0..]
           nonZeroTerms = filter nonZeroCoef zipWithDeg
           termStrings  = map (uncurry showTerm) nonZeroTerms
           reversed     = reverse termStrings
           withPlusSign = intersperse " + " reversed
       in foldl1 (++) withPlusSign
       where nonZeroCoef (num, _) = num /= 0
             showTerm coef 0      = (show coef)
             showTerm 1    1      = "x"
             showTerm (-1) 1      = "-x"
             showTerm coef 1      = (show coef) ++ "x"
             showTerm coef degree = (show coef) ++ "x^" ++ (show degree)

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

