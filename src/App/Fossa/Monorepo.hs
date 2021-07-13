{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Monorepo (
  monorepoMain,
) where

import App.Fossa.EmbeddedBinary
import App.Fossa.ProjectInference
import App.Fossa.VPS.Scan.RunWiggins
import App.Fossa.VPS.Types
import App.Types
import Control.Carrier.Diagnostics
import Control.Effect.Lift (Lift, sendIO)
import Data.String.Conversion (toText)
import Data.Text
import Effect.Exec
import Effect.Logger
import Fossa.API.Types
import System.Exit (exitFailure)

monorepoMain :: BaseDir -> MonorepoAnalysisOpts -> Severity -> ApiOpts -> ProjectMetadata -> OverrideProject -> IO ()
monorepoMain basedir monoRepoAnalysisOpts logSeverity apiOpts projectMeta overrideProject = withDefaultLogger logSeverity $ do
  result <- runDiagnostics $ withWigginsBinary $ monorepoScan basedir monoRepoAnalysisOpts logSeverity apiOpts projectMeta overrideProject
  case result of
    Left failure -> do
      logStdout $ toText $ show $ renderFailureBundle failure
      sendIO exitFailure
    Right _ -> pure ()

monorepoScan :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => BaseDir -> MonorepoAnalysisOpts -> Severity -> ApiOpts -> ProjectMetadata -> OverrideProject -> BinaryPaths -> m ()
monorepoScan (BaseDir basedir) monorepoAnalysisOpts logSeverity apiOpts projectMeta projectOverride binaryPaths = do
  projectRevision <- mergeOverride projectOverride <$> (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
  saveRevision projectRevision

  -- todo: get (followSymlinks ScanType) and filterExpressions from user input
  let scanType = ScanType True False False
  let filterExpressions = FilterExpressions []
  let wigginsOpts = generateWigginsMonorepoOpts basedir monorepoAnalysisOpts logSeverity projectRevision scanType filterExpressions apiOpts projectMeta

  logInfo "Running monorepo scan"
  stdout <- runExecIO $ runWiggins binaryPaths wigginsOpts
  logInfo $ pretty stdout

runWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
runWiggins binaryPaths opts = do
  execWiggins binaryPaths opts
