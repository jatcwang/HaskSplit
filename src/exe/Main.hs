{-|
HaskSplit is a Haskell implementation of Sharmir's Secret Sharing Scheme.
This command line program allows you to split a file into /n/ number of shares (/n/ < 256),
and be able to reconstruct the original file from a subset of these shares.
You can choose the minimum number of shares required to recontruct the 
original file, /k/ (where /k/ <= /n/).
If you attempt to reconstruct the original file with any number of files, but
the output will only be correct when you reconstruct with /k/ or more files.

= Usage

Example command line Usage:

@
hasksplit split splitme.mp3 5 3 -o ./outputFolder
@

You should now see 5 files in the ./outputFolder directory, with name of the form
splitme.mp3.XXX, where XXX is between 001 and 255

to reconstruct:

@
hasksplit reconstruct splitme.mp3 -i .\/outputFolder -o .\/originalFile
@

(the -i options specifies where your file shares are located)

You should now see your original file splitme.mp3 under ./originalFile folder

This program uses:

* /optparse-applicative/ for command line parsing

* /Conduit/ for data streaming / program process pipeline

* /errors/ for error handling
-}
module Main 
    ( main 
    ) where
import HaskSplit.Check
import HaskSplit.Conduit.Split
import HaskSplit.Conduit.Reconstruct
import Control.Error
import Options.Applicative -- for command line argument parsing
import Control.Monad.IO.Class
import System.Directory
import System.FilePath

data Command
    = Split SplitOptions
    | Reconstruct ReconstructOptions

data SplitOptions = SplitOptions 
    { splitInputFile :: FilePath
    , splitTotalShares :: Int
    , splitThreshold :: Int
    , splitOutputDir :: FilePath
    }

data ReconstructOptions = ReconstructOptions
    { reconOrigFileName :: FilePath
    , reconInputDir     :: FilePath
    , reconOutputDir    :: FilePath
    }
    
parseCommands :: Parser Command
parseCommands = 
    subparser $ 
        (command "split" splitCmd
         <> 
         command "sp" splitCmd
         <>
         command "reconstruct" reconstructCmd 
         <>
         command "re" reconstructCmd 
        )
            
     where splitCmd = (info splitOptions (progDesc "Split a file"))
           reconstructCmd = (info reconstructOptions (progDesc "Reconstruct a file from its parts"))
            
splitOptions :: Parser Command
splitOptions = Split <$> 
    (SplitOptions
    <$> argument str (metavar "INPUT_FILE")
    <*> argument auto (metavar "TOTAL_SHARES")
    <*> argument auto (metavar "THRESHOLD")
    <*> (strOption
          ( long "outputdir"
          <> short 'o'
          <> metavar "OUPUT_DIR"
          <> value "."
          <> help "Specify an alternative output directory"
          ))
    )

reconstructOptions :: Parser Command
reconstructOptions = Reconstruct <$>
    (ReconstructOptions
    <$> argument str (metavar "ORIGINAL_FILE_NAME")
    <*> (strOption
        ( long "inputdir"
        <> short 'i'
        <> metavar "INPUT_DIR"
        <> value "."
        <> help "The location of the input files (files used to recontruct the original file)"
        ))
    <*> (strOption
        (long "outputdir"
        <> short 'o'
        <> metavar "OUTPUT_DIR"
        <> value "."
        <> help "Directory to place the reconstructed file"
        ))
    )

runCommand :: Command -> IO ()
runCommand (Split options) = do
    result <- runEitherT (runSplit options)
    case result of
        Left msg -> putStrLn msg
        Right _ -> return ()
runCommand (Reconstruct options) = do
    result <- runEitherT (runReconstruct options)
    case result of 
        Left msg -> putStrLn msg
        Right _ -> return ()

runSplit :: SplitOptions -> EitherT String IO ()
runSplit options = do
    let inFile = takeFileName $ splitInputFile options
        n = splitTotalShares options
        k = splitThreshold options
        outDir = splitOutputDir options
    runPureCheck $ checkSplitParameters n k
    checkFileExists (splitInputFile options)
    liftIO $ createDirectoryIfMissing True outDir
    absOutDir <- liftIO $ canonicalizePath outDir
    liftIO $ splitFile inFile outDir n k
    liftIO $ putStrLn $ "Split Complete.\n" ++
                        "Total Shares: " ++ show n ++ "\n" ++
                        "Files required to reconstruct original: " ++ show k ++ "\n" ++
                        "Output to: " ++ absOutDir

runReconstruct :: ReconstructOptions -> EitherT String IO ()
runReconstruct options = do
    let origFileName = reconOrigFileName options
        outFile = (reconOutputDir options) </> origFileName
        outDir = takeDirectory outFile
        inDir = reconInputDir options
    runPureCheck $ checkName outFile inDir    -- checkName's result is Either, so need hoistEither to lift it to the EitherT monad
    checkDirectoryExists inDir
    splitFiles <- getAllSplitFiles origFileName inDir
    runPureCheck $ checkValidSplitNumber splitFiles
    checkFileSize splitFiles
    liftIO $ createDirectoryIfMissing True outDir
    absOutDir <- liftIO $canonicalizePath outDir
    liftIO $ reconstructFile splitFiles outFile
    liftIO $ putStrLn $ "Reconstruction Complete.\n" ++
                        "Number of input: " ++ show (length splitFiles) ++ "\n" ++ 
                        "Output to: " ++ (absOutDir </> origFileName)

opts :: ParserInfo Command
opts = info (parseCommands <**> helper) idm

main :: IO () 
main = execParser opts >>= runCommand
    

