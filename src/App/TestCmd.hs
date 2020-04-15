module App.TestCmd
  ( testMain
  ) where

import Prologue

import App.Scan.ProjectInference
import Control.Carrier.Error.Either
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as M
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

  result <- timeout testTimeoutSeconds $ withLogger SevInfo $ runError @TestError $ do
    inferred <- inferProject basedir

    let name = inferredName inferred
        revision = inferredRevision inferred
   
    logInfo $ "Using project name: " <> pretty (inferredName inferred)
    logInfo $ "Using project name: " <> pretty (inferredRevision inferred)

    logSticky "[ Waiting for build completion... ]"

    waitForBuild apiKey name revision

    logSticky "[ Waiting for issue scan completion... ]"
    issues <- waitForIssues apiKey name revision
    logSticky ""

    if null (Fossa.issuesIssues issues)
      then logInfo "Test passed! 0 issues found"
      else do
        traverse_ (logError . pretty) (renderedIssues issues)
        throwError NonEmptyIssues

  case result of
    Nothing -> do
      hPutStrLn stderr "Timed out while waiting for issues scan"
      exitFailure
    Just (Left err) -> do
      hPutStrLn stderr $ renderTestError err
      exitFailure
    Just (Right ()) -> pure ()

renderedIssues :: Fossa.Issues -> [Text]
renderedIssues issues = rendered
  where
    issuesList = Fossa.issuesIssues issues

    categorize :: Ord k => (v -> k) -> [v] -> Map k [v]
    categorize f = M.fromListWith (++) . map (\v -> (f v, [v]))

    issuesByType :: Map Fossa.IssueType [Fossa.Issue]
    issuesByType = categorize Fossa.issueType issuesList

    rendered = concatMap (\(ty, iss) -> renderHeader ty ++ map renderIssue iss ++ ["\n"]) (M.toList issuesByType)

    renderHeader ty =
      [ "========================================================================"
      , Fossa.renderIssueType ty
      , "========================================================================"
      , case ty of
          Fossa.IssuePolicyConflict -> "Dependency\tRevision\tLicense"
          Fossa.IssuePolicyFlag -> "Dependency\tRevision\tLicense"
          _ -> "Dependency\tRevision"
      , ""
      ]

    renderIssue :: Fossa.Issue -> Text
    renderIssue issue = revision <> "\t" <> fromMaybe "" (Fossa.ruleLicenseId =<< Fossa.issueRule issue)
      where
        revision =
          case T.split (\c -> c == '$' || c == '+') (Fossa.issueRevisionId issue) of
            (_:name:version:_) -> name <> " " <> version
            (_:name:_) -> name
            _ -> Fossa.issueRevisionId issue

data TestError
  = TestErrorFossa Fossa.FossaError
  | NotFinished Fossa.BuildStatus
  | NonEmptyIssues
  deriving (Show, Generic)

renderTestError :: TestError -> Text
renderTestError (TestErrorFossa err) = "An API error occurred: " <> T.pack (show err)
renderTestError (NotFinished status) = "The build didn't complete in time. Last status: " <> T.pack (show status)
renderTestError NonEmptyIssues = "Test failed: issues found."

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
