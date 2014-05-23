{-# LANGUAGE TemplateHaskell #-}

module Split_test (splitProperties) where

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import TestTypes

import FiniteField.GF256
import HaskSplit.Algorithm
import System.Random

prop_encodeDecode (PGFCoeffs xs) d g' =
    forAll (choose (1, length xs - 1)) $ \degree ->
        let g = mkStdGen g'
            ys = encode xs degree g d 
        in d == decode xs ys
    where
        types = (d :: GF256Elm, g' :: Int)

splitProperties :: [Test]
splitProperties = [ testProperty "Test encode and decode of a GF256 Element" prop_encodeDecode ]
