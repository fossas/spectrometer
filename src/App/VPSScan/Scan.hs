module App.VPSScan.Scan
  ( scanMain
  , ScanCmdOpts(..)
  , VPSOpts(..)
  ) where

import Prologue

import Control.Carrier.Diagnostics
import Effect.Exec
import System.Exit (exitFailure)
import Control.Concurrent.Async (concurrently)
import Control.Carrier.Trace.Printing

import App.VPSScan.Types
import App.VPSScan.Scan.RunSherlock
import App.VPSScan.Scan.ScotlandYard
import App.VPSScan.Scan.RunIPR
import App.VPSScan.Scan.RunS3Upload
import App.Util (validateDir)

data ScanCmdOpts = ScanCmdOpts
  { cmdBasedir :: FilePath
  , scanVpsOpts :: VPSOpts
  } deriving Generic

scanMain :: ScanCmdOpts -> IO ()
scanMain opts@ScanCmdOpts{..} = do
  basedir <- validateDir cmdBasedir
  result <- runDiagnostics $ runTrace $ vpsScan basedir opts
  case result of
    Left failure -> do
      print $ renderFailureBundle failure
      exitFailure
    Right _ -> pure ()

----- main logic

vpsScan ::
  (Has Trace sig m
  , MonadIO m
  , Has Diagnostics sig m
  ) => Path Abs Dir -> ScanCmdOpts -> m ()
vpsScan basedir opts@ScanCmdOpts{..} = do
  let vpsOpts@VPSOpts{..} = scanVpsOpts
  response <- createScotlandYardScan vpsOpts
  let scanId = responseScanId response
  trace $ "Running scan on directory " ++ show basedir
  trace $ "Scan ID from Scotland yard is " ++ show scanId
  trace "[All] Running IPR and Sherlock scans in parallel"
  trace "[Sherlock] Starting Sherlock scan"
  _ <- runScans basedir scanId opts
  trace "[All] All scans complete"

runScans :: (Has Diagnostics sig m, Has Trace sig m, MonadIO m) => Path Abs Dir -> Text -> ScanCmdOpts -> m ()
runScans basedir scanId ScanCmdOpts{..} = do
  let vpsOpts@VPSOpts{..} = scanVpsOpts
  case vpsIpr of
    Just _ -> trace "[IPR] Starting IPR scan"
    Nothing -> trace "[IPR] IPR scan disabled"

  let runIt = runExecIO . runTrace . runDiagnostics
  (iprResult, sherlockResult) <- liftIO $ concurrently
         (runIt $ runIPRScan basedir scanId vpsOpts)
         (runIt $ runSherlockScan basedir scanId vpsOpts)
  trace "[All] Scans complete"
  case (iprResult, sherlockResult) of
    (Left iprFailure, _) -> do
      liftIO $ print $ renderFailureBundle iprFailure
      liftIO exitFailure
    (_, Left sherlockFailure) -> do
      liftIO $ print $ renderFailureBundle sherlockFailure
      liftIO exitFailure
    (_, _) ->
      case vpsIpr of
        Just IPROpts { s3Bucket = Just bucket} -> do
          trace "[S3] Uploading files to S3 for first-party-license review"
          _ <- runTrace $ runS3Upload basedir scanId vpsOpts bucket
          trace "[S3] S3 upload complete"
        Just _ -> do
          trace "[S3] S3 bucket not provided. Skipping S3 upload"
        Nothing ->
          trace "[S3] IPR scan disabled. Skipping S3 upload"

runSherlockScan ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has Trace sig m
  ) => Path Abs Dir -> Text -> VPSOpts -> m ()
runSherlockScan basedir scanId vpsOpts = do
  execSherlock basedir scanId vpsOpts
  trace "[Sherlock] Sherlock scan complete"

runS3Upload ::
  ( Has Diagnostics sig m
  , Has Trace sig m
  , MonadIO m
  ) => Path Abs Dir -> Text -> VPSOpts -> String -> m ()
runS3Upload basedir scanId VPSOpts{..} bucket =
  case vpsIpr of
    Just iprOpts ->
      execS3Upload basedir scanId iprOpts bucket
    Nothing ->
      trace "[S3] S3 upload not required. Skipping"

runIPRScan ::
  ( Has Diagnostics sig m
  , Has Trace sig m
  , Has Exec sig m
  , MonadIO m
  ) => Path Abs Dir -> Text -> VPSOpts -> m ()
runIPRScan basedir scanId vpsOpts@VPSOpts{..} =
  case vpsIpr of
    Just iprOpts -> do
      iprResult <- execIPR basedir iprOpts
      trace "[IPR] IPR scan completed. Posting results to Scotland Yard"

      context "uploading scan results" $ uploadIPRResults vpsOpts scanId iprResult
      trace "[IPR] Post to Scotland Yard complete"
      trace "[IPR] IPR scan complete"
    Nothing ->
      trace "[IPR] IPR Scan disabled"
