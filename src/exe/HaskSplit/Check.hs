-- | File and Directory checks for both the Splitting and reconstruction 
-- of a file.

module HaskSplit.Check 
    ( -- * Split Checks 
      checkSplitParameters
    , checkFileExists
      -- * Reconstruction Checks
    , checkName
    , checkDirectoryExists
    , getAllSplitFiles
    , checkValidSplitNumber
    , checkFileSize
    , getFileSize
      -- * Rephrasing
    , runPureCheck
    ) where
    

import System.FilePath
import System.IO
import System.Directory
import System.FilePath.Glob
import Control.Error
import Control.Monad.IO.Class
import Control.Exception (bracket)

-- | Check both input file name and provided location of file shares
-- are valid file paths strings.
checkName :: FilePath -> FilePath -> Either String ()
checkName fName folder
    | (null fName) = Left $ "Input file name " ++ fName ++ " is not valid" 
    | not (isValid folder) = Left "Invalid location for files"
    | otherwise = Right ()

-- | Check whether directory exist and fails if it doesn't.
checkDirectoryExists :: FilePath -> EitherT String IO ()
checkDirectoryExists folder = do
    exists <- liftIO $ doesDirectoryExist folder
    if exists
        then hoistEither $ Right ()
        else hoistEither $ Left $ "Directory " ++ folder ++ " does not exist"
    
-- | Search through a directory for files that are possible shares of the 
-- original file, using the pattern <Original File Name>.DDD where DDD are 
-- digits. 
-- Fails if no such files are found.
getAllSplitFiles :: FilePath -> FilePath -> EitherT String IO [FilePath]
getAllSplitFiles fName folder = do
    files <- liftIO $ globDir1 pattern folder
    let validFiles = filter ((== fName) . dropExtension . takeFileName) files
        fileNames = map takeFileName validFiles                               -- dropping the folder path at the start
    if (not . null) fileNames
        then hoistEither $ Right validFiles
        else hoistEither $ Left  "No split files exist in provided directory"
        where
            pattern = compile "*.[0-9][0-9][0-9]"

-- | Check the share number obtained from the files are valid (between 1 and
-- 255, inclusive). Fails if any one file fails the check
checkValidSplitNumber :: [FilePath] -> Either String ()
checkValidSplitNumber fileNames = do
    let xs = map (read . lastN 3) fileNames :: [Int]
    if all (\x -> x > 0 && x < 256) xs 
        then Right ()
        else Left $ "One or more split files has invalid share number " ++
                    "(must be between 1 and 255 inclusive)"
    where lastN n lst = drop (length lst - n) lst

-- | Check all file shares have the same size. Fails if they don't.
checkFileSize :: [FilePath] -> EitherT String IO ()
checkFileSize fileNames = do 
    fileSizes <- mapM getFileSize fileNames
    if allTheSame fileSizes 
        then hoistEither $ Right ()
        else hoistEither $ Left "File sizes are not the same"
    where allTheSame xs = all (== head xs) (tail xs)

-- | Get the file size of a file. Used by checkFileSize
getFileSize :: FilePath -> EitherT String IO Integer
getFileSize path = 
    handleT (\_ -> hoistEither $ Left ("Error getting file size of " ++ path)) $
        liftIO $ bracket (openFile path ReadMode) hClose $ \h -> hFileSize h

-- | Check the split parameters. Both numbers must be between 1 and 255
-- (inclusive), and the threshold parameter /k/ cannot exceed the total number
-- of shares (/n/).
checkSplitParameters :: Int -> Int -> Either String ()
checkSplitParameters n k
    | (n < 0) || (n > 255) || (k < 0) || (k > 255) = Left "Both Total Shares and Share Threshold must be between 1 and 255 (inclusive)"
    | k > n                                        = Left "Share Threshold cannot exceed Total Shares"
    | otherwise                                    = Right ()

-- | Check that a file exists
checkFileExists :: FilePath -> EitherT String IO ()
checkFileExists fp = do
    exists <- liftIO $ doesFileExist fp
    if exists 
        then hoistEither $ Right ()
        else hoistEither $ Left ("file " ++ fp ++ " does not exist")
    
-- | A rephrase of @hoistEither@, for clarity that some checks does not involve IO.
runPureCheck :: Monad m => Either e a -> EitherT e m a
runPureCheck = hoistEither
    
