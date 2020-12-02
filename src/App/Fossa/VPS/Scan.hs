{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan
  ( scanMain,
    SkipIPRScan (..),
    LicenseOnlyScan (..)
  ) where

import Control.Effect.Lift (Lift, sendIO)
import Control.Carrier.Diagnostics
import Effect.Exec
import System.Exit (exitFailure)
import Control.Concurrent.Async (concurrently)

import App.Fossa.VPS.EmbeddedBinary
import App.Fossa.VPS.Scan.Core
import App.Fossa.VPS.Scan.RunIPR
import App.Fossa.VPS.Scan.RunSherlock
import App.Fossa.VPS.Scan.RunWiggins
import App.Fossa.VPS.Scan.ScotlandYard
import App.Fossa.VPS.Types
import App.Fossa.ProjectInference
import App.Types (BaseDir (..), OverrideProject (..), ProjectRevision (..), ProjectMetadata (..))
import Data.Aeson
import Data.Flag (Flag, fromFlag)
import Data.Text (pack, Text)
import Effect.Logger
import Path
import Fossa.API.Types (unApiKey, ApiOpts(..))
import System.Environment

-- | SkipIPRScan bool flag
data SkipIPRScan = SkipIPRScan

-- | LicenseOnlyScan bool flag
data LicenseOnlyScan = LicenseOnlyScan

scanMain :: BaseDir -> ApiOpts -> ProjectMetadata -> Severity -> OverrideProject -> FilterExpressions -> Flag SkipIPRScan -> Flag LicenseOnlyScan ->  IO ()
scanMain basedir apiOpts metadata logSeverity overrideProject fileFilters skipIprScan licenseOnlyScan = do
  result <- runDiagnostics $ withEmbeddedBinaries $ vpsScan basedir logSeverity overrideProject skipIprScan licenseOnlyScan fileFilters apiOpts metadata
  case result of
    Left failure -> do
      print $ renderFailureBundle failure
      exitFailure
    Right _ -> pure ()

----- main logic

vpsScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) => BaseDir -> Severity -> OverrideProject -> Flag SkipIPRScan -> Flag LicenseOnlyScan -> FilterExpressions -> ApiOpts -> ProjectMetadata -> BinaryPaths -> m ()
vpsScan (BaseDir basedir) logSeverity overrideProject skipIprFlag licenseOnlyScan fileFilters apiOpts metadata binaryPaths = withLogQueue logSeverity $ \queue -> runLogger queue $ do
  let scanType = ScanType (fromFlag SkipIPRScan skipIprFlag) (fromFlag LicenseOnlyScan licenseOnlyScan)
  let wigginsOpts = generateWigginsOpts basedir logSeverity overrideProject scanType fileFilters apiOpts metadata

  logDebug "Running wiggins plugin scan"
  let runIt = runLogger queue . runDiagnostics . runExecIO

  wigginsResult <- sendIO $ runIt (runWiggins binaryPaths wigginsOpts)
  case wigginsResult of
    (Right _) -> logDebug "Wiggins plugin scan complete"
    (Left wigginsFailure) -> do
      logDebug "Wiggins plugin failed to scan"
      logDebug $ renderFailureBundle wigginsFailure
      sendIO exitFailure

runWiggins :: ( Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m ()
runWiggins binaryPaths opts = do
  execWiggins binaryPaths opts
