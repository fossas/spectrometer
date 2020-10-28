{-# LANGUAGE NumericUnderscores #-}

module App.Fossa.VPS.Test
  ( testMain,
    TestOutputType (..),
  )
where

import App.Fossa.API.BuildWait
import qualified App.Fossa.FossaAPIV1 as Fossa
import App.Fossa.ProjectInference
import qualified App.Fossa.VPS.Scan.Core as VPSCore
import qualified App.Fossa.VPS.Scan.ScotlandYard as ScotlandYard
import App.Types
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Effect.Lift (sendIO)
import qualified Data.Aeson as Aeson
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Effect.Exec
import Effect.Logger
import Fossa.API.Types (ApiOpts)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import Control.Carrier.Lift (Lift)

data TestOutputType
  = -- | pretty output format for issues
    TestOutputPretty
  | -- | use json output for issues
    TestOutputJson

testMain ::
  BaseDir ->
  ApiOpts ->
  Severity ->
  -- | timeout (seconds)
  Int ->
  TestOutputType ->
  OverrideProject ->
  IO ()
testMain basedir apiOpts logSeverity timeoutSeconds outputType override = do
  _ <- timeout timeoutSeconds . withLogger logSeverity . runExecIO $ do
    result <- runDiagnostics $ do
      revision <- mergeOverride override <$> inferProject (unBaseDir basedir)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")

      logSticky "[ Getting latest scan ID ]"

      Fossa.Organization orgId <- Fossa.getOrganization apiOpts
      let locator = VPSCore.createLocator (projectName revision) orgId

      scan <- ScotlandYard.getLatestScan apiOpts locator (projectRevision revision)

      logSticky "[ Waiting for component scan... ]"

      -- FIXME: this should probably call out to the real API
      -- we may not be able to count on stability for the API, though, which is
      -- why we use this command instead
      waitForSherlockScan apiOpts locator (ScotlandYard.responseScanId scan)

      logSticky "[ Waiting for issue scan completion... ]"
      issues <- waitForIssues apiOpts revision
      logSticky ""

      if null (Fossa.issuesIssues issues)
        then logInfo "Test passed! 0 issues found"
        else do
          case outputType of
            TestOutputPretty -> logError $ "Test failed: " <> pretty (length (Fossa.issuesIssues issues)) <> " issues found"
            TestOutputJson -> logStdout . pretty . decodeUtf8 . Aeson.encode $ issues
          sendIO exitFailure

    case result of
      Left failure -> do
        logError $ renderFailureBundle failure
        sendIO exitFailure
      Right _ -> sendIO exitSuccess

  -- we call exitSuccess/exitFailure in each branch above. the only way we get
  -- here is if we time out
  hPutStrLn stderr "Timed out while waiting for issues scan"
  exitFailure

pollDelaySeconds :: Int
pollDelaySeconds = 8

waitForSherlockScan ::
  (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) =>
  ApiOpts ->
  VPSCore.Locator ->
  -- | scan ID
  Text ->
  m ()
waitForSherlockScan apiOpts locator scanId = do
  scan <- ScotlandYard.getScan apiOpts locator scanId
  case ScotlandYard.responseScanStatus scan of
    Just "AVAILABLE" -> pure ()
    Just otherStatus -> do
      logSticky $ "[ Waiting for component scan... last status: " <> pretty otherStatus <> " ]"
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForSherlockScan apiOpts locator scanId
    Nothing -> do
      sendIO $ threadDelay (pollDelaySeconds * 1_000_000)
      waitForSherlockScan apiOpts locator scanId

timeout ::
  -- | number of seconds before timeout
  Int ->
  IO a ->
  IO (Maybe a)
timeout seconds act = either id id <$> Async.race (Just <$> act) (threadDelay (seconds * 1_000_000) $> Nothing)
