{-# LANGUAGE RankNTypes #-}

-- | IO implementation for splitting a file using
-- Conduit, a streaming library.

module HaskSplit.Conduit.Split 
    ( splitFile
    ) where

import System.Random (StdGen, split, newStdGen)
import Data.Conduit
import Data.Conduit.Binary as CB hiding (drop)
import qualified Data.Conduit.Combinators as CC (map, concat, concatMap)
import Control.Monad.Trans.Resource (runResourceT, MonadResource(..))
import qualified Data.ByteString as BS
import GHC.Word (Word8(..))
import Data.MonoTraversable 
import Safe (atMay)
import Control.Applicative
import Control.Monad.IO.Class
import Text.Printf
import HaskSplit.Algorithm
import FiniteField.GF256 (GF256Elm(..))
import HaskSplit.Util
import System.FilePath

-- | An infinite source of seeds which are used to generate random numbers
sourceStdGen :: MonadIO m => Source m StdGen
sourceStdGen = do
    g <- liftIO newStdGen
    loop g
    where loop gin = do
            let g' = fst (split gin)
            yield gin
            loop g'

-- | Create a source for splitting a file
-- The source will split the input file into individual bytes, and yielding each byte along with a RNG seed
sourceSplit :: (MonadResource m, MonadIO m) => FilePath -> Source m (StdGen, Word8)
sourceSplit fp = getZipSource $ (,)
    <$> ZipSource sourceStdGen
    <*> ZipSource (sourceFile fp $= CC.concat)

-- | Encode a byte using Shamir's Secret Sharing Scheme. The output is a list of bytes, 
-- with each byte corresponding to each file share
encodeByte :: [Int] -> Int -> (StdGen, Word8) -> [Word8]
encodeByte xs deg (g, d) =
    map (fromIntegral . getInt) $ encode (map GF256Elm xs) deg g (fromIntegral d)
    
-- | Get the sink file, given a filepath generator (that takes an Int) and the split number
idxSinkFile :: MonadResource m
            => [FilePath]
            -> Int
            -> Consumer [Word8] m ()
idxSinkFile outFileNames shareNumber =
    CC.concatMap (flip atMay shareNumber) =$= CC.map BS.singleton =$= sinkFile (outFileNames !! shareNumber)

-- | Generate a sink which will take a list of bytes and write each byte to its corresponding file share
sinkMultiFiles :: MonadResource m
               => [FilePath]
               -> [Int]
               -> Sink [Word8] m ()
sinkMultiFiles outFileNames xs =
   getZipSink $ otraverse_ (ZipSink . idxSinkFile outFileNames) [0..length xs - 1]

-- | Generate the output file paths from the original file name, output directory,
-- and the x value of each file
-- e.g. genOutputFileNames "input.txt" "./outputDir" [22, 55]
-- will give you ["./outputDir/input.txt.022", "./output/to/input.txt.055"]
genOutputFileNames :: FilePath -> FilePath -> [Int] -> [FilePath]
genOutputFileNames fileName outDir xs = 
    map (\fileNumb -> outDir </> fileName ++ (printf ".%03d" fileNumb)) xs

-- | Split a file into n different shares, where k of these shares can 
-- be used to successfully reconstruct the original file
splitFile :: FilePath -> FilePath -> Int -> Int -> IO ()
splitFile inFile outDir n k = do
    g <- newStdGen
    let pool = [1..255] :: [Int]
    let xs = uniqueRandom g n pool
        degree = k - 1
        outFileNames = genOutputFileNames inFile outDir xs
        sink = sinkMultiFiles outFileNames xs
    runResourceT $ sourceSplit inFile $$ (CC.map (encodeByte xs degree)) =$ sink

