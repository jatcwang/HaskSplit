module HaskSplit.Algorithm
    ( encode
    , decode
    ) where

import System.Random
import FiniteField.PGF256
import FiniteField.GF256
import HaskSplit.Util
import Prelude hiding ((/))

encode :: [GF256Elm] -> Int -> StdGen -> GF256Elm -> [GF256Elm]
encode xs degree g d = 
    let poly = d:(generateCoeffs degree g)     -- the randomly generated polynomial
    in map (calc poly) xs

decode :: [GF256Elm] -> [GF256Elm] -> GF256Elm
decode xs ys  =
    let len   = length xs
        termF = (\i -> getTerm xs ys i)
    in sum $ map termF [0..(len-1)]

-- get the ith term of lagrange's interpolation polynomial
getTerm :: [GF256Elm] -> [GF256Elm] -> Int -> GF256Elm
getTerm xs ys i = 
    let topX = nfoldr1 (*) xs i
        botX = product $ nmap ((-) (xs !! i)) xs i
        res  = (ys !! i) * (topX / botX)
    in res
    
generateCoeffs :: Int -> StdGen -> [GF256Elm]
generateCoeffs 0 _ = []
generateCoeffs degree g = 
    let (r, nextG)   = toGF256Elm $ randomR (1, 255) g
    in generateCoeffs' (degree - 1) [r] nextG

    where generateCoeffs' :: Int -> [GF256Elm] -> StdGen -> [GF256Elm]
          generateCoeffs' 0 coeffs _ = coeffs                         
          generateCoeffs' deg coeffs g =                           
              let (r, nextG)   = toGF256Elm $ randomR (1, 255) g
              in generateCoeffs' (deg - 1) (r:coeffs) nextG        

          toGF256Elm :: (Integer, StdGen) -> (GF256Elm, StdGen)
          toGF256Elm (a, g) = (fromInteger a, g)
              

