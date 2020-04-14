module App.TestCmd
  ( testMain
  ) where

import Prologue

import App.Scan.ProjectInference
import Control.Effect.Lift
import qualified Data.Text as T
import Effect.Logger
import qualified App.Scan.FossaV1 as Fossa
import Path.IO
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout, stderr)
import System.Environment (getEnv)

testMain :: IO ()
testMain = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  basedir <- getCurrentDir

  -- FIXME
  apiKey <- getEnv "FOSSA_API_KEY"
  test basedir (T.pack apiKey)
    & withLogger SevInfo

test ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Effect sig
  , MonadIO m
  )
  => Path Abs Dir
  -> Text -- ^ api key
  -> m ()
test basedir key = do
  inferred <- inferProject basedir

  logInfo $ "Using project name: " <> pretty (inferredName inferred)
  logInfo $ "Using project name: " <> pretty (inferredRevision inferred)
 
  logSticky "[ Waiting for build completion... ]"
 
  status <- liftIO $ Fossa.waitForBuild key (inferredName inferred) (inferredRevision inferred)
  case status of
    Left err -> do
      logError $ "Error occurred when checking for build completion"
      logError $ viaShow err
      --liftIO $ exitFailure
    Right Fossa.StatusSucceeded -> pure ()
    Right otherStatus -> do
      logError $ "Timed out waiting for build completion. Most recent status: " <> viaShow otherStatus
      --liftIO $ exitFailure
 
  logSticky "[ Waiting for issue scan... ]"

  issues <- liftIO $ Fossa.waitForIssues key (inferredName inferred) (inferredRevision inferred)
  case issues of
    Left err -> do
      logError $ "Error occurred when checking for issue scan completion"
      logError $ viaShow err
    Right (Fossa.Issues { Fossa.issuesIssues = issuesList, Fossa.issuesStatus = "SCANNED" }) -> do
      if null issuesList
        then logInfo "Test passed! 0 issues found"
        else logError "Test failed: issues found"
    Right _ -> do
      logError $ "Timed out waiting for issue scan completion"
      --liftIO $ exitFailure

  logSticky ""
