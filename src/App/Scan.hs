
module App.Scan
  ( scanMain
  , ScanCmdOpts(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import Control.Effect.Exception as Exc
import Control.Carrier.Output.IO
import Control.Concurrent
import qualified Data.Sequence as S
import Path.IO
import System.Exit (die)

import App.Scan.Project (mkProjects)
import App.Scan.ProjectInference (InferredProject(..), inferProject)
import Control.Carrier.TaskPool
import Control.Carrier.Threaded
import qualified Data.ByteString.Lazy as BL
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Discovery
import Effect.Logger
import Types

data ScanCmdOpts = ScanCmdOpts
  { cmdBasedir :: FilePath
  , cmdDebug   :: Bool
  , cmdOutFile :: Maybe FilePath
  } deriving (Eq, Ord, Show, Generic)

scanMain :: ScanCmdOpts -> IO ()
scanMain ScanCmdOpts{..} = do
  basedir <- validateDir cmdBasedir

  scan basedir cmdOutFile
    & withLogger (bool SevInfo SevDebug cmdDebug)
    & runThreaded

validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute

scan :: forall sig m.
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

  let runIt discover = discoverFunc discover basedir

  (closures,()) <- runOutput @ProjectClosure $
    withTaskPool capabilities updateProgress (traverse_ runIt discoverFuncs)

  logSticky "[ Combining Analyses ]"

  let projects = mkProjects strategyGroups (S.fromList closures)
  liftIO $ case outFile of
    Nothing -> BL.putStr (encode projects)
    Just path -> liftIO (encodeFile path projects)

  inferred <- inferProject basedir
  logInfo ""
  logInfo ("Inferred project name: `" <> pretty (inferredName inferred) <> "`")
  logInfo ("Inferred revision: `" <> pretty (inferredRevision inferred) <> "`")

  logSticky ""

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
