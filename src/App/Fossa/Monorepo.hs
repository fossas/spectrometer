module App.Fossa.Monorepo (
  monorepoMain,
  toPathFilters,
  PathFilters (..),
) where

import App.Fossa.EmbeddedBinary
import App.Fossa.ProjectInference
import App.Fossa.VPS.Scan.RunWiggins
import App.Types
import Control.Carrier.Diagnostics
import Control.Effect.Lift (Lift)
import Data.Text
import Discovery.Filters
import Effect.Exec
import Effect.Logger
import Fossa.API.Types

-- Monorepo scans support a subset of AllFilters: specifically, target filters are not supported as monorepo scans do not have the concept of targets.
toPathFilters :: AllFilters -> PathFilters
toPathFilters AllFilters{includeFilters, excludeFilters} = PathFilters (combinedPaths includeFilters) (combinedPaths excludeFilters)

monorepoMain :: BaseDir -> MonorepoAnalysisOpts -> Severity -> ApiOpts -> ProjectMetadata -> OverrideProject -> PathFilters -> IO ()
monorepoMain basedir monoRepoAnalysisOpts logSeverity apiOpts projectMeta overrideProject filters = withDefaultLogger logSeverity $ do
  logWithExit_ $ withWigginsBinary $ monorepoScan basedir monoRepoAnalysisOpts filters logSeverity apiOpts projectMeta overrideProject

monorepoScan :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => BaseDir -> MonorepoAnalysisOpts -> PathFilters -> Severity -> ApiOpts -> ProjectMetadata -> OverrideProject -> BinaryPaths -> m ()
monorepoScan (BaseDir basedir) monorepoAnalysisOpts filters logSeverity apiOpts projectMeta projectOverride binaryPaths = do
  projectRevision <- mergeOverride projectOverride <$> (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
  saveRevision projectRevision

  let wigginsOpts = generateWigginsMonorepoOpts basedir monorepoAnalysisOpts filters logSeverity projectRevision apiOpts projectMeta

  logInfo "Running monorepo scan"
  stdout <- context "Monorepo" $ runExecIO $ runWiggins binaryPaths wigginsOpts
  logInfo $ pretty stdout

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
runWiggins binaryPaths opts = do
  context "Running monorepo binary" $ execWiggins binaryPaths opts
