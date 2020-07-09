module App.VPSScan.Main
  ( appMain
  ) where

import Prologue

import Options.Applicative

import App.VPSScan.Scan (ScanCmdOpts(..), scanMain)
import App.VPSScan.NinjaGraph (NinjaGraphCmdOpts(..), ninjaGraphMain)
import App.VPSScan.Types
import qualified App.VPSScan.Scan.RunIPR as RunIPR
import OptionExtensions

appMain :: IO ()
appMain = join (customExecParser (prefs showHelpOnEmpty) opts)

opts :: ParserInfo (IO ())
opts = info (commands <**> helper) (fullDesc <> header "vpscli -- FOSSA Vendored Package Scan CLI")

commands :: Parser (IO ())
commands = hsubparser $ scanCommand <> ninjaGraphCommand

vpsOpts :: Parser VPSOpts
vpsOpts = VPSOpts <$> runSherlockOpts <*> optional runIPROpts <*> syOpts <*> organizationIDOpt <*> projectIDOpt <*> revisionIDOpt
            where
              organizationIDOpt = option auto (long "organization" <> metavar "orgID" <> help "Organization ID")
              projectIDOpt = strOption (long "project" <> metavar "String" <> help "Project ID")
              revisionIDOpt = strOption (long "revision" <> metavar "String" <> help "Revision ID")

ninjaGraphOpts :: Parser NinjaGraphOpts
ninjaGraphOpts = NinjaGraphOpts <$> ninjaDepsOpt <*> lunchTargetOpt <*> scotlandYardUrlOpt
                 where
                   ninjaDepsOpt = optional $ strOption (long "ninjadeps" <> metavar "STRING")
                   lunchTargetOpt = optional $ strOption (long "lunchtarget" <> metavar "STRING" <> help "build target name to pass to lunch. If you are running in an environment with envsetup and lunch already configured, then you don't need to pass this in")
                   scotlandYardUrlOpt = uriOption (long "scotland-yard-url" <> metavar "STRING" <> help "URL for Scotland Yard service")

runSherlockOpts :: Parser SherlockOpts
runSherlockOpts = SherlockOpts
                  <$> sherlockCmdPathOpt
                  <*> sherlockUrlOpt
                  <*> sherlockClientTokenOpt
                  <*> sherlockClientIDOpt
                where
                    sherlockCmdPathOpt = strOption (long "sherlock-cli" <> metavar "STRING" <> help "Path to the sherlock-cli executable")
                    sherlockUrlOpt = strOption(long "sherlock-url" <> metavar "STRING" <> help "URL for Sherlock service")
                    sherlockClientTokenOpt = strOption(long "client-token" <> metavar "STRING" <> help "Client token for authentication to Sherlock")
                    sherlockClientIDOpt = strOption(long "client-id" <> metavar "STRING" <> help "Client ID for authentication to Sherlock")

-- If all three of --ipr, --nomossa and --pathfinder are provided, then we run an IPR scan
-- If none of them are provided, then we skip the IPR scan
-- Providing just one or two of --ipr, --nomossa or --pathfinder will result in an error
-- If an IPR scan is run and --s3-bucket is provided, then we upload the scanned files to an S3 or S3 compatible (E.g. Minio) instance
runIPROpts :: Parser RunIPR.IPROpts
runIPROpts = RunIPR.IPROpts
                  <$> iprCmdPathOpt
                  <*> nomosCmdPathOpt
                  <*> pathfinderCmdPathOpt
                  <*> s3Bucket
                  <*> s3Endpoint
                where
                    iprCmdPathOpt =  strOption (long "ipr" <> metavar "STRING" <> help "Path to the IPR executable")
                    nomosCmdPathOpt = strOption (long "nomossa" <> metavar "STRING" <> help "Path to the nomossa executable")
                    pathfinderCmdPathOpt = strOption (long "pathfinder" <> metavar "STRING" <> help "Path to the pathfinder executable")
                    s3Bucket = optional $ strOption (long "s3-bucket" <> metavar "STRING" <> help "Bucket to upload first-party scan files on S3 or S3 equivalent")
                    s3Endpoint = optional $ uriOption (long "s3-endpoint" <> metavar "STRING" <> help "optional URL for S3 service. If not provided, will default to https://s3.amazonaws.com. You can specify an S3 region using a URL like https://s3.us-west-2.amazonaws.com, or a Minio service iwth a URL like http://localhost:9000.")

-- org IDs are ints. Project and revision IDs are strings
syOpts :: Parser ScotlandYardOpts
syOpts = ScotlandYardOpts
                     <$> scotlandYardUrlOpt
                  where
                    scotlandYardUrlOpt = uriOption (long "scotland-yard-url" <> metavar "STRING" <> help "URL for Scotland Yard service")

basedirOpt :: Parser FilePath
basedirOpt = strOption (long "basedir" <> short 'd' <> metavar "DIR" <> help "Base directory for scanning" <> value ".")

scanCommand :: Mod CommandFields (IO ())
scanCommand = command "scan" (info (scanMain <$> scanOptsParser) (progDesc "Scan for projects and their dependencies"))
  where
  scanOptsParser = ScanCmdOpts
                   <$> basedirOpt
                   <*> vpsOpts

ninjaGraphCommand :: Mod CommandFields (IO ())
ninjaGraphCommand = command "ninja-graph" (info (ninjaGraphMain <$> ninjaGraphOptsParser) (progDesc "Get a dependency graph for a ninja build"))
  where
    ninjaGraphOptsParser = NinjaGraphCmdOpts
                          <$> basedirOpt
                          <*> ninjaGraphOpts
