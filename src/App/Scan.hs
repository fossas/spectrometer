
module App.Scan
  ( scanMain
  , ScanCmdOpts(..)
  , VPSOpts(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import Path.IO
import System.Exit (exitFailure, die)

import Network.HTTP.Req (HttpException)

import VPSScan.RunSherlock
import VPSScan.ScotlandYard
import VPSScan.RunIPR

data ScanCmdOpts = ScanCmdOpts
  { cmdBasedir :: FilePath
  , cmdDebug   :: Bool
  , cmdOutFile :: Maybe FilePath
  , scanVpsOpts :: VPSOpts
  } deriving (Eq, Ord, Show, Generic)

data VPSOpts = VPSOpts
  { vpsSherlock :: SherlockOpts
  , vpsIpr :: IPROpts
  , vpsScotlandYard :: ScotlandYardOpts
  } deriving (Eq, Ord, Show, Generic)

      --putStrLn "Starting VPS scan"
      --let scotlandYardOpts = vpsScotlandYard vpsOpts
          --iprOpts = vpsIpr vpsOpts
          --sherlockOpts = vpsSherlock vpsOpts
      --putStrLn "about to hit Scotland Yard "
      --scanResponse <- ScotlandYard.runHTTP (ScotlandYard.createScan scotlandYardOpts)
      --putStrLn $ "Scan response: " ++ show scanResponse
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
  ) => Path Abs Dir -> ScanCmdOpts -> m ()
vpsScan basedir ScanCmdOpts{..} = do
  let VPSOpts{..} = scanVpsOpts
  response <- tagError Couldn'tGetScanId =<< createScotlandYardScan vpsScotlandYard
 
  traceM $ "Running scan on directory " ++ show basedir
  let scanId = responseScanId response
  traceM $ "Scan ID from Scotland yard is " ++ show scanId
  traceM "Starting IPR scan"
  iprResult <- tagError IPRFailed =<< execIPR basedir vpsIpr
  traceM "IPR scan completed. Posting results to Scotland Yard"
  tagError Couldn'tUpload =<< uploadIPRResults vpsScotlandYard scanId iprResult
  traceM "Running Sherlock scan"
  tagError SherlockFailed =<< execSherlock basedir scanId vpsSherlock
  traceM "Scan complete"

tagError :: Has (Error e') sig m => (e -> e') -> Either e a -> m a
tagError f (Left e) = throwError (f e)
tagError _ (Right a) = pure a

validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute
