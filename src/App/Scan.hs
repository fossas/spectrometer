
module App.Scan
  ( scanMain
  , ScanCmdOpts(..)
  , VPSOpts(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import Control.Effect.Exception as Exc
import Control.Carrier.Output.IO
import Control.Concurrent
import qualified Data.Sequence as S
import Path.IO
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout, stderr)
import System.Exit (die)

import App.Scan.Project (mkProjects)
import App.Scan.ProjectInference (InferredProject(..), inferProject)
import Control.Carrier.TaskPool
import Control.Carrier.Threaded
import qualified Data.ByteString.Lazy as BL
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Effect.Exec (ExecErr(..), runExecIO)
import Effect.Logger
import Effect.ReadFS (ReadFSErr(..))
import Network.HTTP.Req (HttpException)

import qualified VPSScan.RunSherlock as RunSherlock
import qualified VPSScan.ScotlandYard as ScotlandYard
import qualified VPSScan.RunIPR as RunIPR
import qualified Strategy.Carthage as Carthage
import qualified Strategy.Cocoapods.Podfile as Podfile
import qualified Strategy.Cocoapods.PodfileLock as PodfileLock
import qualified Strategy.Go.GoList as GoList
import qualified Strategy.Go.Gomod as Gomod
import qualified Strategy.Go.GopkgLock as GopkgLock
import qualified Strategy.Go.GopkgToml as GopkgToml
import qualified Strategy.Go.GlideLock as GlideLock
import qualified Strategy.Gradle as Gradle
import qualified Strategy.Maven.Pom as MavenPom
import qualified Strategy.Maven.PluginStrategy as MavenPlugin
import qualified Strategy.Node.NpmList as NpmList
import qualified Strategy.Node.NpmLock as NpmLock
import qualified Strategy.Node.PackageJson as PackageJson
import qualified Strategy.Node.YarnLock as YarnLock
import qualified Strategy.NuGet.PackagesConfig as PackagesConfig
import qualified Strategy.NuGet.PackageReference as PackageReference
import qualified Strategy.NuGet.ProjectAssetsJson as ProjectAssetsJson
import qualified Strategy.NuGet.ProjectJson as ProjectJson
import qualified Strategy.NuGet.Nuspec as Nuspec
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy
import qualified Strategy.Ruby.BundleShow as BundleShow
import qualified Strategy.Ruby.GemfileLock as GemfileLock
import Types

data ScanCmdOpts = ScanCmdOpts
  { cmdBasedir :: FilePath
  , cmdDebug   :: Bool
  , cmdOutFile :: Maybe FilePath
  , scanVpsOpts :: Maybe VPSOpts
  } deriving (Eq, Ord, Show, Generic)

data VPSOpts = VPSOpts
  { vpsSherlock :: RunSherlock.SherlockOpts
  , vpsIpr :: RunIPR.IPROpts
  , vpsScotlandYard :: ScotlandYard.ScotlandYardOpts
  } deriving (Eq, Ord, Show, Generic)

scanMain :: ScanCmdOpts -> IO ()
scanMain ScanCmdOpts{..} = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  basedir <- validateDir cmdBasedir
  case scanVpsOpts of
    Nothing -> undefined
    Just vpsOpts ->  do
      putStrLn "Starting VPS scan"
      let scotlandYardOpts = vpsScotlandYard vpsOpts
          iprOpts = vpsIpr vpsOpts
          sherlockOpts = vpsSherlock vpsOpts
      putStrLn "about to hit Scotland Yard " 
      scanResponse <- ScotlandYard.runHTTP (ScotlandYard.createScan scotlandYardOpts)
      putStrLn $ "Scan response: " ++ show scanResponse

      _ <- case scanResponse of
        Left _ -> pure $ Right ()
        Right sResponse ->  do
          let s = ScotlandYard.scanId sResponse
          putStrLn $ "scanId: " ++ show s
          _ <- runExecIO $ runError @ExecErr $ runError @HttpException $ RunIPR.scan basedir s scotlandYardOpts iprOpts
          _ <- runExecIO $ runError @ExecErr $ RunSherlock.scan basedir s sherlockOpts
          pure $ Right ()
      pure ()

  -- let runScan = scan basedir cmdOutFile
  --       & withLogger (bool SevInfo SevDebug cmdDebug)
  --       & runThreaded
  -- runScan

validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute

scan ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Threaded sig m
  , MonadIO m
  , Effect sig
  )
  => Path Abs Dir -> Maybe FilePath -> m ()
scan basedir outFile = do
  setCurrentDir basedir
  capabilities <- liftIO getNumCapabilities

  (closures,(failures,())) <- runOutput @ProjectClosure $ runOutput @ProjectFailure $
    withTaskPool capabilities updateProgress (traverse_ ($ basedir) discoverFuncs)

  logSticky "[ Combining Analyses ]"

  let result = buildResult closures failures
  liftIO $ case outFile of
    Nothing -> BL.putStr (encode result)
    Just path -> liftIO (encodeFile path result)

  inferred <- inferProject basedir
  logInfo ""
  logInfo ("Inferred project name: `" <> pretty (inferredName inferred) <> "`")
  logInfo ("Inferred revision: `" <> pretty (inferredRevision inferred) <> "`")

  logSticky ""

buildResult :: [ProjectClosure] -> [ProjectFailure] -> Value
buildResult closures failures = object
  [ "projects" .= mkProjects (S.fromList closures)
  , "failures" .= map renderFailure failures
  ]

renderFailure :: ProjectFailure -> Value
renderFailure failure = object
  [ "name" .= projectFailureName failure
  , "cause" .= renderCause (projectFailureCause failure)
  ]

renderCause :: SomeException -> Value
renderCause e = fromMaybe renderSomeException $
      renderReadFSErr <$> fromException e
  <|> renderExecErr   <$> fromException e
  where
  renderSomeException = object
    [ "type" .= ("unknown" :: Text)
    , "err"  .= show e
    ]

  renderReadFSErr :: ReadFSErr -> Value
  renderReadFSErr = \case
    FileReadError path err -> object
      [ "type" .= ("file_read_error" :: Text)
      , "path" .= path
      , "err"  .= err
      ]
    FileParseError path err -> object
      [ "type" .= ("file_parse_error" :: Text)
      , "path" .= path
      , "err"  .= err
      ]
    ResolveError base path err -> object
      [ "type" .= ("file_resolve_error" :: Text)
      , "base" .= base
      , "path" .= path
      , "err"  .= err
      ]

  renderExecErr :: ExecErr -> Value
  renderExecErr = \case
    CommandFailed cmd outerr -> object
      [ "type"   .= ("command_execution_error" :: Text)
      , "cmd"    .= cmd
      , "stderr" .= outerr
      ]
    CommandParseError cmd err -> object
      [ "type" .= ("command_parse_error" :: Text)
      , "cmd"  .= cmd
      , "err"  .= err
      ]

discoverFuncs :: HasDiscover sig m => [Path Abs Dir -> m ()]
discoverFuncs =
  [ GoList.discover
  , Gomod.discover
  , GopkgToml.discover
  , GopkgLock.discover
  , GlideLock.discover

  , Gradle.discover

  , MavenPlugin.discover
  , MavenPom.discover

  , PackageJson.discover
  , NpmLock.discover
  , NpmList.discover
  , YarnLock.discover

  , PackagesConfig.discover
  , PackageReference.discover
  , ProjectAssetsJson.discover
  , ProjectJson.discover
  , Nuspec.discover

  , Pipenv.discover
  , SetupPy.discover
  , ReqTxt.discover

  , BundleShow.discover
  , GemfileLock.discover

  , Carthage.discover

  , Podfile.discover
  , PodfileLock.discover
  ]

updateProgress :: Has Logger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )
