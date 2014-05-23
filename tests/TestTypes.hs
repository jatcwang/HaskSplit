module TestTypes where 

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Applicative
import Data.List
import FiniteField.GF256

-- TestNum - generate number between 0 and 255 inclusive

genInt :: Gen Integer
genInt = elements [0..255]

instance Arbitrary GF256Elm where
    arbitrary = (GF256Elm . fromIntegral) <$> genInt

newtype PGFCoeffs = PGFCoeffs {pgfCoeffs :: [GF256Elm]}
    deriving (Show)

-- a list of GF256Elm with no duplicate
instance Arbitrary PGFCoeffs where
    arbitrary = do
        n <- choose (2,400) :: Gen Int
        v <- vectorOf n (arbitrary :: Gen GF256Elm)
        -- let v2 = fmap nub $ filter (/= (GF256Elm 0)) v
        let v2 = filter (/= (GF256Elm 0)) (nub v)
        return $ PGFCoeffs v2

