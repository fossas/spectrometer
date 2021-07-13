{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Monorepo (
  monorepoMain,
) where

import App.Fossa.EmbeddedBinary
import App.Types
import Control.Carrier.Diagnostics
import Control.Effect.Lift (Lift, sendIO)
import Data.String.Conversion (toText)
import Effect.Logger
import Fossa.API.Types
import System.Exit (exitFailure)

monorepoMain :: BaseDir -> MonorepoAnalysisOpts -> Severity -> ApiOpts -> ProjectMetadata -> OverrideProject -> IO ()
monorepoMain baseDir monoRepoAnalysisOpts logSeverity apiOpts projectMeta overrideProject = withDefaultLogger logSeverity $ do
  result <- runDiagnostics $ withWigginsBinary $ monorepoScan baseDir monoRepoAnalysisOpts apiOpts projectMeta overrideProject
  case result of
    Left failure -> do
      logStdout $ toText $ show $ renderFailureBundle failure
      sendIO exitFailure
    Right _ -> pure ()

monorepoScan :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => BaseDir -> MonorepoAnalysisOpts -> ApiOpts -> ProjectMetadata -> OverrideProject -> BinaryPaths -> m ()
monorepoScan = undefined