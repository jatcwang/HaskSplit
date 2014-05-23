-- | Tests for HaskSplit library (pure) functions

module Main (main) where

import Split_test (splitProperties)
import GF256_test (gf256properties)
import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain [ 
            testGroup "split properties"    splitProperties
        ,   testGroup "GF256Elm properties" gf256properties
        ]
