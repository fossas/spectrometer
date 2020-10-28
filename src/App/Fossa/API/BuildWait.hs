{-# LANGUAGE NumericUnderscores #-}

module App.Fossa.API.BuildWait
  ( waitForBuild,
    waitForIssues,
  )
where

import qualified App.Fossa.FossaAPIV1 as Fossa
import App.Types
import Control.Carrier.Diagnostics
import Control.Concurrent (threadDelay)
import Control.Effect.Lift (Lift, sendIO)
import Effect.Logger
import Fossa.API.Types (ApiOpts)

pollDelaySeconds :: Int
pollDelaySeconds = 8

data WaitError
  = -- | we encountered the FAILED status on a build
    BuildFailed
  deriving (Eq, Ord, Show)

instance ToDiagnostic WaitError where
  renderDiagnostic BuildFailed = "The build failed. Check the FOSSA webapp for more details."

waitForBuild ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m ()
waitForBuild apiOpts revision = do
  build <- Fossa.getLatestBuild apiOpts revision

  case Fossa.buildTaskStatus (Fossa.buildTask build) of
    Fossa.StatusSucceeded -> pure ()
    Fossa.StatusFailed -> fatal BuildFailed
    otherStatus -> do
      logSticky $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForBuild apiOpts revision

waitForIssues ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Fossa.Issues
waitForIssues apiOpts revision = do
  issues <- Fossa.getIssues apiOpts revision
  case Fossa.issuesStatus issues of
    "WAITING" -> do
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForIssues apiOpts revision
    _ -> pure issues
