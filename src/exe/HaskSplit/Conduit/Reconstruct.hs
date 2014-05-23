-- | IO implementation of reconstructing a file from
-- several file shares, using Conduit, a streaming library

module HaskSplit.Conduit.Reconstruct 
    ( reconstructFile
    ) where

import Data.Conduit
import Data.Conduit.Binary as CB hiding (drop)
import qualified Data.Conduit.Combinators as CC (map, concat)
import Control.Monad.Trans.Resource (runResourceT, MonadResource(..))
import GHC.Word (Word8(..))
import Data.ByteString (ByteString, singleton)
import Control.Monad.IO.Class
import HaskSplit.Algorithm
import FiniteField.GF256 (GF256Elm(..))
import Data.Traversable
import System.FilePath

-- | Generate a Source for reconstructing a file from multiple input files
sourceReconstruct :: (MonadResource m, MonadIO m) => [FilePath] -> Source m [Word8]
sourceReconstruct filePaths =
    getZipSource $ traverse (\fp -> ZipSource (sourceFile fp $= CC.concat)) filePaths

-- | Generate a Conduit, which calculates the byte to be written to the output 
-- file from the bytes from each file share.
conduitReconstruct :: (Monad m) => [Int] -> Conduit [Word8] m ByteString
conduitReconstruct xs =
    CC.map ((singleton . fromIntegral . getInt) 
    . decode (map GF256Elm xs) 
    . map (GF256Elm . fromIntegral))

-- | Reconstruct the original file from several file shares
reconstructFile :: [FilePath] -> FilePath -> IO ()
reconstructFile inFiles outFile = do
    let xs = getXs inFiles
    runResourceT $ 
        sourceReconstruct inFiles 
        $= conduitReconstruct xs 
        $$ sinkFile outFile

-- | Given the file shares, get their x point from their file extension
getXs :: [FilePath] -> [Int]
getXs = map (read . drop 1 . takeExtension)

