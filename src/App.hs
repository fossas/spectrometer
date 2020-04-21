
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
                  <*> sherlockApiKeyOpt
                  <*> sherlockUrlOpt
                  <*> sherlockClientTokenOpt
                  <*> sherlockSecretOpt
                where
                    sherlockCmdPathOpt = strOption (long "sherlock-cmd-path" <> metavar "STRING" <> help "Path to the sherlock-cli executable (only necessary for vendored package scans)")
                    sherlockApiKeyOpt = strOption (long "sherlock-api-key" <> metavar "STRING" <> help "API key for Sherlock API (only necessary for vendored package scans)")
                    sherlockUrlOpt = strOption(long "sherlock-url" <> metavar "STRING" <> help "URL for Sherlock service (only necessary for vendored package scans)")
                    sherlockClientTokenOpt = strOption(long "sherlock-client-token" <> metavar "STRING" <> help "Client token for authentication to Sherlock (only necessary for vendored package scans)")
                    sherlockSecretOpt = strOption(long "sherlock-secret" <> metavar "STRING" <> help "Shared secret for authentication to Sherlock (only necessary for vendored package scans)")

runIPROpts :: Parser (RunIPR.IPROpts)
runIPROpts = RunIPR.IPROpts
                  <$> iprCmdPathOpt
                  <*> nomosCmdPathOpt
                  <*> pathfinderCmdPathOpt
                where
                    iprCmdPathOpt = strOption (long "ipr-cmd-path" <> metavar "STRING" <> help "Path to the IPR executable (only necessary for vendored package scans)")
                    nomosCmdPathOpt = strOption (long "nomos-cmd-path" <> metavar "STRING" <> help "Path to the nomossa executable (only necessary for vendored package scans)")
                    pathfinderCmdPathOpt = strOption (long "pathfinder-cmd-path" <> metavar "STRING" <> help "Path to the pathfinder executable (only necessary for vendored package scans)")

syOpts :: Parser (ScotlandYard.ScotlandYardOpts)
syOpts = ScotlandYard.ScotlandYardOpts
                     <$> scotlandYardUrlOpt
                     <*> organizationIDOpt
                     <*> projectIDOpt
                     <*> revisionIDOpt
                  where
                    scotlandYardUrlOpt = strOption(long "scotland-yard-url" <> metavar "STRING" <> help "URL for Scotland Yard service (only necessary for vendored package scans)")
                    organizationIDOpt = strOption (long "organization-id" <> metavar "STRING" <> help "Organization ID (only necessary for vendored package scans)")
                    projectIDOpt = strOption (long "project-id" <> metavar "String" <> help "Project ID (only necessary for vendored package scans")
                    revisionIDOpt = strOption (long "revision-id" <> metavar "String" <> help "Project ID (only necessary for vendored package scans")

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