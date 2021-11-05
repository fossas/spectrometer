module App.Fossa.Monorepo (
  monorepoMain,
  toPathFilters,
  PathFilters,
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, withWigginsBinary)
import App.Fossa.ProjectInference (
  inferProjectDefault,
  inferProjectFromVCS,
  mergeOverride,
  saveRevision,
 )
import App.Fossa.VPS.Scan.RunWiggins (
  PathFilters,
  WigginsOpts,
  execWiggins,
  generateWigginsMonorepoOpts,
  toPathFilters,
 )
import App.Types (
  BaseDir (BaseDir),
  MonorepoAnalysisOpts,
  OverrideProject,
  ProjectMetadata,
 )
import Control.Carrier.Diagnostics (
  Diagnostics,
  Has,
  context,
  logWithExit_,
  (<||>),
 )
import Control.Effect.Lift (Lift)
import Data.Text (Text)
import Effect.Exec (Exec, runExecIO)
import Effect.Logger (
  Logger,
  Pretty (pretty),
  Severity,
  logInfo,
  withDefaultLogger,
 )
import Effect.ReadFS (ReadFS, runReadFSIO)
import Fossa.API.Types (ApiOpts)

monorepoMain ::
  BaseDir ->
  MonorepoAnalysisOpts ->
  Severity ->
  ApiOpts ->
  ProjectMetadata ->
  OverrideProject ->
  PathFilters ->
  IO ()
monorepoMain basedir monoRepoAnalysisOpts logSeverity apiOpts projectMeta overrideProject filters = withDefaultLogger logSeverity $ do
  logWithExit_ $ runReadFSIO $ runExecIO $ withWigginsBinary $ monorepoScan basedir monoRepoAnalysisOpts filters logSeverity apiOpts projectMeta overrideProject

monorepoScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  BaseDir ->
  MonorepoAnalysisOpts ->
  PathFilters ->
  Severity ->
  ApiOpts ->
  ProjectMetadata ->
  OverrideProject ->
  BinaryPaths ->
  m ()
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
