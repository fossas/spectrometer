
module App.Fossa.Main
  ( appMain
  ) where

import Prologue

import App.Fossa.Test
import Effect.Logger
import Options.Applicative
import Path.IO (resolveDir', doesDirExist, setCurrentDir)
import System.Environment (lookupEnv)
import System.Exit (die)
import qualified Data.Text as T

appMain :: IO ()
appMain = do
  CmdOptions{..} <- customExecParser (prefs showHelpOnError) (info (opts <**> helper) (fullDesc <> header "fossa-cli - Flexible, performant dependency analysis"))
  let logSeverity = bool SevInfo SevDebug optDebug

  maybeApiKey <- fmap T.pack <$> lookupEnv "FOSSA_API_KEY"

  basedir <- validateDir optBasedir
 
  setCurrentDir basedir

  case optCommand of
    AnalyzeCommand{} -> undefined
   
    TestCommand TestOptions{..} ->
      case maybeApiKey of
        Nothing -> die "A FOSSA API key is required to run this command"
        Just key -> testMain key logSeverity testTimeout

-- | Validate that a filepath points to a directory and the directory exists
validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute

opts :: Parser CmdOptions
opts =
  CmdOptions
    <$> switch (long "debug" <> help "Enable debug logging")
    <*> strOption (long "basedir" <> short 'd' <> metavar "DIR" <> help "Set the base directory for scanning (default: current directory)" <> value ".")
    <*> comm
    <**> helper

comm :: Parser Command
comm = hsubparser
  ( command "analyze"
    ( info
        (AnalyzeCommand <$> analyzeOpts)
        (progDesc "Scan for projects and their dependencies")
    )
  <> command "test"
    ( info
        (TestCommand <$> testOpts)
        (progDesc "Check for issues from FOSSA and exit non-zero when issues are found")
    )
  )

analyzeOpts :: Parser AnalyzeOptions
analyzeOpts =
  AnalyzeOptions
    <$> switch (long "output" <> short 'o' <> help "Output results to stdout instead of uploading to fossa")

testOpts :: Parser TestOptions
testOpts =
  TestOptions
    <$> option auto (long "timeout" <> help "Duration to wait for build completion (in seconds)" <> value 600)

data CmdOptions = CmdOptions
  { optDebug   :: Bool
  , optBasedir :: FilePath
  , optCommand :: Command
  }

data Command
  = AnalyzeCommand AnalyzeOptions
  | TestCommand TestOptions

data AnalyzeOptions = AnalyzeOptions
  { analyzeOutput :: Bool
  }

data TestOptions = TestOptions
  { testTimeout :: Int
  }
