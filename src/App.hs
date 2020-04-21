
module App
  ( appMain
  ) where

import Prologue

import Options.Applicative

import App.Scan (ScanCmdOpts(..), scanMain)
import qualified VPSScan.RunSherlock as RunSherlock
import qualified VPSScan.RunIPR as RunIPR
import qualified VPSScan.ScotlandYard as ScotlandYard

appMain :: IO ()
appMain = join (customExecParser (prefs showHelpOnEmpty) opts)

opts :: ParserInfo (IO ())
opts = info (commands <**> helper) (fullDesc <> header "hscli - fossa-cli, but functional")

commands :: Parser (IO ())
commands = hsubparser scanCommand

runSherlockOpts :: Parser (RunSherlock.SherlockOpts)
runSherlockOpts = RunSherlock.SherlockOpts
                  <$> sherlockCmdPathOpt
                  <*> sherlockUrlOpt
                  <*> sherlockClientTokenOpt
                  <*> sherlockClientIDOpt
                where
                    sherlockCmdPathOpt = strOption (long "sherlock-cli" <> metavar "STRING" <> help "Path to the sherlock-cli executable (only necessary for vendored package scans)")
                    sherlockUrlOpt = strOption(long "sherlock-url" <> metavar "STRING" <> help "URL for Sherlock service (only necessary for vendored package scans)")
                    sherlockClientTokenOpt = strOption(long "client-token" <> metavar "STRING" <> help "Client token for authentication to Sherlock (only necessary for vendored package scans)")
                    sherlockClientIDOpt = strOption(long "client-id" <> metavar "STRING" <> help "Client ID for authentication to Sherlock (only necessary for vendored package scans)")

runIPROpts :: Parser (RunIPR.IPROpts)
runIPROpts = RunIPR.IPROpts
                  <$> iprCmdPathOpt
                  <*> nomosCmdPathOpt
                  <*> pathfinderCmdPathOpt
                where
                    iprCmdPathOpt = strOption (long "ipr" <> metavar "STRING" <> help "Path to the IPR executable (only necessary for vendored package scans)")
                    nomosCmdPathOpt = strOption (long "nomossa" <> metavar "STRING" <> help "Path to the nomossa executable (only necessary for vendored package scans)")
                    pathfinderCmdPathOpt = strOption (long "pathfinder" <> metavar "STRING" <> help "Path to the pathfinder executable (only necessary for vendored package scans)")

syOpts :: Parser (ScotlandYard.ScotlandYardOpts)
syOpts = ScotlandYard.ScotlandYardOpts
                     <$> scotlandYardUrlOpt
                     <*> organizationIDOpt
                     <*> projectIDOpt
                     <*> revisionIDOpt
                  where
                    scotlandYardUrlOpt = strOption(long "scotland-yard-url" <> metavar "STRING" <> help "URL for Scotland Yard service (only necessary for vendored package scans)")
                    organizationIDOpt = strOption (long "organization" <> metavar "STRING" <> help "Organization ID (only necessary for vendored package scans)")
                    projectIDOpt = strOption (long "project" <> metavar "String" <> help "Project ID (only necessary for vendored package scans")
                    revisionIDOpt = strOption (long "revision" <> metavar "String" <> help "Project ID (only necessary for vendored package scans")

scanCommand :: Mod CommandFields (IO ())
scanCommand = command "scan" (info (scanMain <$> scanOptsParser) (progDesc "Scan for projects and their dependencies"))
  where
  scanOptsParser = ScanCmdOpts
                   <$> basedirOpt
                   <*> debugOpt
                   <*> outputOpt
                   <*> optional runSherlockOpts
                   <*> optional runIPROpts
                   <*> optional syOpts

  basedirOpt = strOption (long "basedir" <> short 'd' <> metavar "DIR" <> help "Base directory for scanning" <> value ".")
  debugOpt = switch (long "debug" <> help "Enable debug logging")
  outputOpt = optional (strOption (long "outfile" <> short 'o' <> metavar "FILE" <> help "Output results to a file (default: stdout). Relative paths are relative to the scan root."))