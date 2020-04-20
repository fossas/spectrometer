
module App.Hscli.Main
  ( appMain
  ) where

import Prologue

import Options.Applicative

import App.Hscli.Scan (ScanCmdOpts(..), scanMain)
import App.Hscli.TestCmd (testMain)

appMain :: IO ()
appMain = join (customExecParser (prefs showHelpOnEmpty) opts)

opts :: ParserInfo (IO ())
opts = info (commands <**> helper) (fullDesc <> header "hscli - fossa-cli, but functional")

commands :: Parser (IO ())
commands = hsubparser (scanCommand <> testCommand)

scanCommand :: Mod CommandFields (IO ())
scanCommand = command "scan" (info (scanMain <$> scanOptsParser) (progDesc "Scan for projects and their dependencies"))
  where
  scanOptsParser = ScanCmdOpts <$> basedirOpt <*> debugOpt <*> outputOpt <*> uploadOpt

  basedirOpt = strOption (long "basedir" <> short 'd' <> metavar "DIR" <> help "Base directory for scanning" <> value ".")
  debugOpt = switch (long "debug" <> help "Enable debug logging")
  outputOpt = optional (strOption (long "outfile" <> short 'o' <> metavar "FILE" <> help "Output results to a file (default: stdout). Relative paths are relative to the scan root."))
  uploadOpt = optional (strOption (long "upload" <> metavar "APIKEY" <> help "Upload results to Fossa"))

testCommand :: Mod CommandFields (IO ())
testCommand = command "test" (info (pure testMain) (progDesc "Check for issues from FOSSA and exit non-zero when issues are found"))
