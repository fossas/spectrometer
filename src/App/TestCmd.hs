module App.TestCmd
  ( testMain
  ) where

import Prologue

import App.Scan.ProjectInference
import Control.Carrier.Error.Either
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Data.Text as T
import Data.Text.IO (hPutStrLn)
import Effect.Logger
import qualified App.Scan.FossaV1 as Fossa
import Path.IO
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout, stderr)
import System.Environment (getEnv)
import System.Exit (exitFailure)

testTimeoutSeconds :: Int
testTimeoutSeconds = 600

pollDelaySeconds :: Int
pollDelaySeconds = 8

testMain :: IO ()
testMain = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  basedir <- getCurrentDir
 
  -- FIXME
  apiKey <- T.pack <$> getEnv "FOSSA_API_KEY"

  result <- timeout testTimeoutSeconds $ runError @TestError $ withLogger SevInfo $ do
    inferred <- inferProject basedir

    let name = inferredName inferred
        revision = inferredRevision inferred
   
    logInfo $ "Using project name: " <> pretty (inferredName inferred)
    logInfo $ "Using project name: " <> pretty (inferredRevision inferred)

    logSticky "[ Waiting for build completion... ]"

    waitForBuild apiKey name revision

    logSticky "[ Waiting for issue scan completion... ]"
    issues <- waitForIssues apiKey name revision

    if null (Fossa.issuesIssues issues)
      then logInfo "Test passed! 0 issues found"
      else logError "Test failed: issues found"

    logSticky ""

  case result of
    Nothing -> do
      hPutStrLn stderr "Timed out while waiting for issues scan"
      exitFailure
    Just (Left err) -> do
      hPutStrLn stderr "An error occurred while retrieving issues scan"
      hPutStrLn stderr $ T.pack (show err)
      exitFailure
    Just (Right ()) -> pure ()


data TestError
  = TestErrorFossa Fossa.FossaError
  | NotFinished Fossa.BuildStatus
  deriving (Show, Generic)

waitForBuild
  :: (Has (Error TestError) sig m, MonadIO m, Has Logger sig m)
  => Text -- ^ api key
  -> Text -- ^ name
  -> Text -- ^ revision
  -> m ()
waitForBuild key name revision = do
  maybeBuild <- liftIO $ Fossa.getLatestBuild key name revision
  case maybeBuild of
    Left err -> throwError (TestErrorFossa err)
    Right build -> do
      case Fossa.buildTaskStatus (Fossa.buildTask build) of
        Fossa.StatusSucceeded -> pure ()
        otherStatus -> do
          logSticky $ "[ Waiting for build completion... last status: " <> viaShow otherStatus <> " ]"
          liftIO $ threadDelay (pollDelaySeconds * 1_000_000)
          waitForBuild key name revision

waitForIssues
  :: (Has (Error TestError) sig m, MonadIO m, Has Logger sig m)
  => Text -- ^ api key
  -> Text -- ^ name
  -> Text -- ^ revision
  -> m Fossa.Issues
waitForIssues key name revision = do
  result <- liftIO $ Fossa.getIssues key name revision
  case result of
    Left err -> throwError (TestErrorFossa err)
    Right issues ->
      case Fossa.issuesStatus issues of
        "WAITING" -> waitForIssues key name revision
        _ -> pure issues

timeout
  :: Int -- ^ number of seconds before timeout
  -> IO a
  -> IO (Maybe a)
timeout seconds act = either id id <$> Async.race (Just <$> act) (threadDelay (seconds * 1_000_000) *> pure Nothing)
