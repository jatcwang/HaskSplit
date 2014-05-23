-- | Calculations for Galois Field (GF256) polynomials

module FiniteField.PGF256 
    ( calc
    ) where

import FiniteField.GF256
import Control.Applicative 

-- | Given the coefficients of a GF256Elm Polynomial and an x value,
-- calculate its corresponding y value.
-- coefficients are in ascending order [d, a1, a2, a3...] where P = d + a1*x + a2*x^2 ...
calc :: [GF256Elm] -> GF256Elm -> GF256Elm
calc coeffs x =
    let pl = powerList (length coeffs - 1) x
    in (sum . getZipList) $ (*) <$> ZipList pl <*> ZipList coeffs

-- | provided the polynomial degree and the x value, produce a list of the form
-- | [1, x^1, x^2, x^3]
powerList :: Int -> GF256Elm -> [GF256Elm]
powerList i x = powerList' i x []
    where powerList' :: Int -> GF256Elm -> [GF256Elm] -> [GF256Elm]       
          powerList' i x []      = powerList' i x [GF256Elm 1]
          powerList' 0 _ xs      = reverse xs
          powerList' i x (xh:xs) = powerList' (i-1) x ((x * xh):xh:xs)

-- xxx testing function
calc' :: Integer -> [Integer] -> Integer
calc' x coeffs = 
    let c = map fromInteger coeffs :: [GF256Elm]
        xx = fromInteger x :: GF256Elm
    in (fromIntegral . getInt) $ calc c xx 

