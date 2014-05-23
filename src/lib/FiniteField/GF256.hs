module FiniteField.GF256 
    ( GF256Elm(..)
    , (/)
    ) where

import Data.Bits ((.&.), xor, shift, testBit)
import Data.Vector as V
import Prelude hiding ((/))

-- | A Galois Field element. Possible value of 0..255 (inclusive)
newtype GF256Elm = GF256Elm {getInt :: Int}
    deriving (Eq, Ord, Read, Show)

type VectInt = V.Vector Int

instance Num GF256Elm where

    GF256Elm a + GF256Elm b = GF256Elm (a `xor` b)

    GF256Elm a - GF256Elm b = GF256Elm (a `xor` b)

    -- Conversion from Integer to GF256Elm, allows any positive or negative integer
    fromInteger a = GF256Elm $ ((fromInteger a `mod` 256) + 256) `mod` 256

    GF256Elm a * GF256Elm b 
        | (a == 0 || b == 0) = GF256Elm 0
        | otherwise          = GF256Elm $ gf256ExpTable ! (((gf256LogTable ! a + gf256LogTable ! b) `mod` 255))

    -- unused. Needed to complete the instance declaration
    signum _ = 1

    -- unused. Needed to complete the instance declaration
    abs a = a

(/) :: GF256Elm -> GF256Elm -> GF256Elm
GF256Elm a / GF256Elm b
    | a == 0    = GF256Elm 0
    | b == 0    = error "Divison by Zero"
    | otherwise = 
        let temp = (gf256LogTable ! a - gf256LogTable ! b)
        in GF256Elm (gf256ExpTable ! ((temp `mod` 255) + 255 `mod` 255)) -- take care of negatives

-- | Function used to calculate the exp and log table of 
-- GF256Elm, which is used for multiplication and division
gf256Init :: Int -> Int -> (VectInt, VectInt) -> (VectInt, VectInt)
gf256Init 255 _ (expTable, logTable) = 
    let expTableNew = expTable // [(255, 0)]
        logTableNew = logTable // [(0, 0)]
    in (expTableNew, logTableNew)
gf256Init i exp (expTable, logTable) = 
    let e = exp .&. 0xff
        expTableNew = expTable // [(i, e)]
        d = testBit e 7 -- check if it's going to be 'over' the max value once multiplied
        e2 = shift e 1 --shifts left, i.e. (x^7, x^6, .., x^0) * (1 0)
        e3 = if (d == True)
                then e2 `xor` 0x1b
                else e2
        e4 = e3 `xor` (expTableNew ! i) -- adding the shifted to its unshifted version.
                                        -- i.e. completing the 3 multiplication
                                        -- since now we have (x^7, x^6 .. x^0) * (1 1)
        logTableNew = logTable // [((expTableNew ! i), i)]
    in gf256Init (i+1) e4 (expTableNew, logTableNew)

-- | Generate the exp and log table using gf256Init
generateTables :: (VectInt, VectInt)
generateTables = gf256Init 0 1 ((V.replicate 256 0), (V.replicate 256 0)) 

-- | GF256Elm log table used for multiplication and division
gf256LogTable :: VectInt
gf256LogTable = snd generateTables

-- | GF256Elm exp table used for multiplication and division
gf256ExpTable :: VectInt
gf256ExpTable = fst generateTables
