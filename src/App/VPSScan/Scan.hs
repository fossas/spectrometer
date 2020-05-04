module App.VPSScan.Scan
  ( scanMain
  , ScanCmdOpts(..)
  , VPSOpts(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import Control.Carrier.Trace.Printing
import Path.IO
import System.Exit (exitFailure, die)
import Control.Concurrent
import Control.Concurrent.Async (concurrently_)

import Network.HTTP.Req (HttpException)

import App.VPSScan.Types
import App.VPSScan.Scan.RunSherlock
import App.VPSScan.Scan.ScotlandYard
import App.VPSScan.Scan.RunIPR

data ScanCmdOpts = ScanCmdOpts
  { cmdBasedir :: FilePath
  , scanVpsOpts :: VPSOpts
  } deriving Generic

scanMain :: ScanCmdOpts -> IO ()
scanMain opts@ScanCmdOpts{..} = do
  basedir <- validateDir cmdBasedir
  result <- runError @VPSError $ runScotlandYard $ runSherlock $ runIPR $ vpsScan basedir opts
  case result of
    Left err -> do
      print err
      exitFailure
    Right _ -> pure ()

----- main logic

data VPSError
  = IPRFailed IPRError
  | SherlockFailed SherlockError
  | Couldn'tGetScanId HttpException
  | Couldn'tUpload HttpException
  deriving (Show, Generic)

vpsScan ::
  ( Has ScotlandYard sig m
  , Has IPR sig m
  , Has Sherlock sig m
  , Has (Error VPSError) sig m
  ) => Path Abs Dir -> ScanCmdOpts -> m (IO ())
vpsScan basedir ScanCmdOpts{..} = do
  let vpsOpts@VPSOpts{..} = scanVpsOpts
  response <- tagError Couldn'tGetScanId =<< createScotlandYardScan vpsOpts
  let scanId = responseScanId response

  pure $ putStrLn $ "Running scan on directory " ++ show basedir
  pure $ putStrLn $ "Scan ID from Scotland yard is " ++ show scanId
  pure $ putStrLn "[All] Running IPR and Sherlock scans in parallel"
  pure $ concurrently_
    (pure $ runError @VPSError $ runIPR $ runScotlandYard $ runIPRScan basedir scanId vpsOpts)
    (pure $ runSherlock $ runError @VPSError $ runSherlockScan basedir scanId vpsOpts)
  pure $ putStrLn "[All] Scans complete"

runSherlockScan ::
  ( Has Sherlock sig m
  , Has (Error VPSError) sig m
  ) => Path Abs Dir -> Text -> VPSOpts -> m (IO ())
runSherlockScan basedir scanId vpsOpts = do
  pure $ putStrLn "[Sherlock] Starting Sherlock scan"
  tagError SherlockFailed =<< execSherlock basedir scanId vpsOpts
  pure $ putStrLn "[Sherlock] Sherlock scan complete"

runIPRScan ::
  ( Has ScotlandYard sig m
  , Has IPR sig m
  , Has (Error VPSError) sig m
  ) => Path Abs Dir ->  Text -> VPSOpts -> m (IO ())
runIPRScan basedir scanId vpsOpts@VPSOpts{..} = do
  pure $ putStrLn "[IPR] Starting IPR scan"

  iprResult <- tagError IPRFailed =<< execIPR basedir vpsIpr
  pure $ putStrLn "[IPR] IPR scan completed. Posting results to Scotland Yard"

  tagError Couldn'tUpload =<< uploadIPRResults vpsOpts scanId iprResult
  pure $ putStrLn "[IPR] Post to Scotland Yard complete"
  pure $ putStrLn "[IPR] IPR scan complete"

tagError :: Has (Error e') sig m => (e -> e') -> Either e a -> m a
tagError f (Left e) = throwError (f e)
tagError _ (Right a) = pure a

validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute
