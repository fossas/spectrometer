{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ListTargets (
  listTargetsMain,
) where

import App.Fossa.Analyze (DiscoverFunc (DiscoverFunc), discoverFuncs)
import App.Fossa.Analyze.Types (AnalyzeExperimentalPreferences)
import App.Types (BaseDir (BaseDir))
import Control.Carrier.AtomicCounter (
  AtomicCounter,
  Has,
  runAtomicCounter,
 )
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Finally (runFinally)
import Control.Carrier.Reader (Reader, runReader)
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool (
  Progress (Progress, pCompleted, pQueued, pRunning),
  TaskPool,
  withTaskPool,
 )
import Control.Concurrent (getNumCapabilities)
import Control.Effect.Debug (Debug)
import Control.Effect.Lift (Lift)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON)
import Data.Aeson.Extra (encodeJSONToText)
import Data.Foldable (for_, traverse_)
import Data.Set qualified as Set
import Data.Set.NonEmpty (toSet)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec, runExecIO)
import Effect.Logger (
  Color (Cyan, Green, Yellow),
  Logger,
  Pretty (pretty),
  Severity (SevInfo),
  annotate,
  color,
  logDebug,
  logInfo,
  withDefaultLogger,
 )
import Effect.ReadFS (ReadFS, runReadFSIO)
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (makeRelative)
import Types (
  BuildTarget (unBuildTarget),
  DiscoveredProject (projectBuildTargets, projectData, projectPath, projectType),
  FoundTargets (FoundTargets, ProjectWithoutTargets),
 )

listTargetsMain :: AnalyzeExperimentalPreferences -> Severity -> BaseDir -> IO ()
listTargetsMain preferences logSeverity (BaseDir basedir) = do
  capabilities <- getNumCapabilities

  ignoreDebug
    . withDefaultLogger logSeverity
    . runStickyLogger SevInfo
    . runFinally
    . withTaskPool capabilities updateProgress
    . runReadFSIO
    . runExecIO
    . runAtomicCounter
    . runReader preferences
    $ runAll basedir

runAll ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has TaskPool sig m
  , Has (Lift IO) sig m
  , MonadIO m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has (Reader AnalyzeExperimentalPreferences) sig m
  ) =>
  Path Abs Dir ->
  m ()
runAll basedir = traverse_ single discoverFuncs
  where
    single (DiscoverFunc f) = withDiscoveredProjects f basedir (printSingle basedir)

printSingle :: (ToJSON a, Has Logger sig m) => Path Abs Dir -> DiscoveredProject a -> m ()
printSingle basedir project = do
  let maybeRel = makeRelative basedir (projectPath project)

  case maybeRel of
    Nothing -> pure ()
    Just rel -> do
      logInfo $
        "Found project: "
          <> pretty (projectType project)
          <> "@"
          <> pretty (toFilePath rel)

      logDebug . pretty . encodeJSONToText $ projectData project

      case projectBuildTargets project of
        ProjectWithoutTargets -> do
          logInfo $
            "Found target: "
              <> pretty (projectType project)
              <> "@"
              <> pretty (toFilePath rel)
        FoundTargets targets -> for_ (Set.toList $ toSet targets) $ \target -> do
          logInfo $
            "Found target: "
              <> pretty (projectType project)
              <> "@"
              <> pretty (toFilePath rel)
              <> ":"
              <> pretty (unBuildTarget target)

updateProgress :: Has StickyLogger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky'
    ( "[ "
        <> annotate (color Cyan) (pretty pQueued)
        <> " Waiting / "
        <> annotate (color Yellow) (pretty pRunning)
        <> " Running / "
        <> annotate (color Green) (pretty pCompleted)
        <> " Completed"
        <> " ]"
    )
