module App.Fossa.VPS.Report
  ( reportMain
  , ReportType (..)
  ) where

import App.Fossa.API.BuildWait
import qualified App.Fossa.FossaAPIV1 as Fossa
import App.Fossa.ProjectInference
import App.Types
import Control.Carrier.Diagnostics
import Control.Effect.Lift (sendIO)
import qualified Data.Aeson as Aeson
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effect.Logger
import Effect.ReadFS
import Fossa.API.Types (ApiOpts)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import qualified App.Fossa.VPS.Scan.Core as VPSCore
import qualified App.Fossa.VPS.Scan.ScotlandYard as ScotlandYard
import qualified Console.Sticky as Sticky

data ReportType =
    AttributionReport

reportName :: ReportType -> Text
reportName r = case r of
  AttributionReport -> "attribution"

reportMain ::
  BaseDir
  -> ApiOpts
  -> Severity
  -> Int -- ^ timeout (seconds)
  -> ReportType
  -> OverrideProject
  -> IO ()
reportMain (BaseDir basedir) apiOpts logSeverity timeoutSeconds reportType override = do
  -- TODO: refactor this code duplicate from `fossa test`
  {-
  Most of this module (almost everything below this line) has been copied
  from App.Fossa.Test.  I wanted to push this out sooner, and refactoring
  everything right away was not appropriate for the timing of this command.

  Main points of refactor:
  * Waiting for builds and issue scans (separately, but also together)
    * Above includes errors, types, and scaffolding
  * Timeout over `IO a` (easy to move, but where do we move it?)
  * CLI command refactoring as laid out in https://github.com/fossas/issues/issues/129
  -}
  void $ timeout timeoutSeconds $ withDefaultLogger logSeverity $ do
    result <- runDiagnostics . runReadFSIO . Sticky.withStickyRegion $ \region -> do
      revision <- mergeOverride override <$> (inferProjectFromVCS basedir <||> inferProjectCached basedir <||> inferProjectDefault basedir)

      Sticky.setSticky region "[ Getting latest scan ID ]"

      Fossa.Organization orgId _ <- Fossa.getOrganization apiOpts
      let locator = VPSCore.createLocator (projectName revision) orgId

      scan <- ScotlandYard.getLatestScan apiOpts locator (projectRevision revision)

      Sticky.setSticky region "[ Waiting for component scan... ]"

      waitForSherlockScan region apiOpts locator (ScotlandYard.responseScanId scan)

      Sticky.setSticky region "[ Waiting for issue scan completion... ]"
      _ <- waitForIssues region apiOpts revision

      Sticky.setSticky' region $ "[ Fetching " <> pretty (reportName reportType) <> " report... ]"
      jsonValue <- case reportType of
        AttributionReport ->
          Fossa.getAttributionRaw apiOpts revision

      logStdout . pretty . decodeUtf8 $ Aeson.encode jsonValue

    case result of
      Left err -> do
        logError $ renderFailureBundle err
        sendIO exitFailure
      Right _ -> sendIO exitSuccess

  hPutStrLn stderr "Timed out while waiting for build/issues scan"
  exitFailure
