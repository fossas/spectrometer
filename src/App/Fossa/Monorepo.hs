module App.Fossa.Monorepo (
  monorepoMain,
  toPathFilters,
  PathFilters (..),
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, withWigginsBinary)
import App.Fossa.FossaAPIV1 (featureFlagEnabled)
import App.Fossa.ProjectInference (inferProjectDefault, inferProjectFromVCS, mergeOverride, saveRevision)
import App.Fossa.VPS.Scan.RunWiggins (PathFilters (..), WigginsOpts, execWiggins, generateWigginsMonorepoOpts, toPathFilters)
import App.Types (BaseDir (..), FeatureFlag (..), MonorepoAnalysisOpts, OverrideProject, ProjectMetadata)
import Control.Carrier.Diagnostics (Diagnostics, Has, context, fatalText, logWithExit_, recover, (<||>))
import Control.Effect.Lift (Lift)
import Data.Text (Text)
import Effect.Exec (Exec, runExecIO)
import Effect.Logger (Logger, Pretty (..), Severity, logInfo, withDefaultLogger)
import Fossa.API.Types (ApiOpts)

monorepoMain :: BaseDir -> MonorepoAnalysisOpts -> Severity -> ApiOpts -> ProjectMetadata -> OverrideProject -> PathFilters -> IO ()
monorepoMain basedir monoRepoAnalysisOpts logSeverity apiOpts projectMeta overrideProject filters = withDefaultLogger logSeverity $ do
  logWithExit_ $ withWigginsBinary $ monorepoScan basedir monoRepoAnalysisOpts filters logSeverity apiOpts projectMeta overrideProject

monorepoScan :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => BaseDir -> MonorepoAnalysisOpts -> PathFilters -> Severity -> ApiOpts -> ProjectMetadata -> OverrideProject -> BinaryPaths -> m ()
monorepoScan (BaseDir basedir) monorepoAnalysisOpts filters logSeverity apiOpts projectMeta projectOverride binaryPaths = do
  -- The endpoint to disable this function if Monorepo analysis is disabled server-side is new.
  -- For most feature flags, we'd want to default to the flag *not* enabled.
  -- However in the case of Monorepo, we want to default to the flag being enabled if we fail to check it in Core to maintain existing functionality.
  enabled <- recover $ featureFlagEnabled apiOpts FeatureFlagVSIMonorepo
  if enabled == Just False
    then fatalText "Monorepo analysis is not enabled in FOSSA. Please contact FOSSA for assistance."
    else pure ()

  projectRevision <- mergeOverride projectOverride <$> (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
  saveRevision projectRevision

  let wigginsOpts = generateWigginsMonorepoOpts basedir monorepoAnalysisOpts filters logSeverity projectRevision apiOpts projectMeta

  logInfo "Running monorepo scan"
  stdout <- context "Monorepo" $ runExecIO $ runWiggins binaryPaths wigginsOpts
  logInfo $ pretty stdout

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
runWiggins binaryPaths opts = do
  context "Running monorepo binary" $ execWiggins binaryPaths opts
