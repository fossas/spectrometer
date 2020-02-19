
module App.Scan
  ( scanMain
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
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Discovery
import Effect.Logger
import Types

scanMain :: Path Abs Dir -> Bool -> IO ()
scanMain basedir debug = do
  exists <- doesDirExist basedir
  unless exists (die $ "ERROR: " <> show basedir <> " does not exist")

  scan basedir
    & withLogger (bool SevInfo SevDebug debug)
    & runThreaded

scan :: forall sig m.
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Threaded sig m
  , MonadIO m
  , Effect sig
  )
  => Path Abs Dir -> m ()
scan basedir = do
  setCurrentDir basedir
  capabilities <- liftIO getNumCapabilities

  let runIt discover = discoverFunc discover basedir

  (closures,()) <- runOutput @ProjectClosure $
    withTaskPool capabilities updateProgress (traverse_ runIt discoverFuncs)

  logSticky "[ Combining Analyses ]"

  let projects = mkProjects strategyGroups (S.fromList closures)
  liftIO (encodeFile "analysis.json" projects)

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
