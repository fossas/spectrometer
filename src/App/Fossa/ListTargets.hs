{-# LANGUAGE RecordWildCards #-}

module App.Fossa.ListTargets
  ( listTargetsMain,
  )
where

import App.Fossa.Analyze (discoverFuncs)
import App.Types (BaseDir (..))
import Console.Sticky qualified as Sticky
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Finally
import Control.Carrier.TaskPool
import Control.Concurrent (getNumCapabilities)
import Control.Effect.Lift
import Data.Foldable (for_)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Path (toFilePath)
import Path.IO (makeRelative)
import Types (BuildTarget (..), DiscoveredProject (..))

type DummyM = ReadFSIOC (ExecIOC (Diag.DiagnosticsC IO))

listTargetsMain :: BaseDir -> IO ()
listTargetsMain (BaseDir basedir) = Sticky.withStickyRegion $ \region -> do
  capabilities <- getNumCapabilities

  withDefaultLogger SevInfo
    . runFinally
    . withTaskPool capabilities (updateProgress region)
    . runReadFSIO
    . runExecIO
    $ do
      withDiscoveredProjects discoverFuncs False basedir $ \(project :: DiscoveredProject DummyM) -> do
        let maybeRel = makeRelative basedir (projectPath project)

        case maybeRel of
          Nothing -> pure ()
          Just rel -> do
            logInfo $
              "Found project: "
                <> pretty (projectType project)
                <> "@"
                <> pretty (toFilePath rel)

            for_ (projectBuildTargets project) $ \target -> do
              logInfo $
                "Found target: "
                  <> pretty (projectType project)
                  <> "@"
                  <> pretty (toFilePath rel)
                  <> ":"
                  <> pretty (unBuildTarget target)

updateProgress :: (Has (Lift IO) sig m, Has Logger sig m) => Sticky.StickyRegion -> Progress -> m ()
updateProgress region Progress {..} =
  Sticky.setSticky'
    region
    ( "[ "
        <> annotate (color Cyan) (pretty pQueued)
        <> " Waiting / "
        <> annotate (color Yellow) (pretty pRunning)
        <> " Running / "
        <> annotate (color Green) (pretty pCompleted)
        <> " Completed"
        <> " ]"
    )
