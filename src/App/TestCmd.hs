module App.TestCmd
  ( testMain
  ) where

import Prologue
import qualified Prelude as Unsafe

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
        logError (renderedIssues issues)
        throwError NonEmptyIssues

  case result of
    Nothing -> do
      hPutStrLn stderr "Timed out while waiting for issues scan"
      exitFailure
    Just (Left err) -> do
      hPutStrLn stderr $ renderTestError err
      exitFailure
    Just (Right ()) -> pure ()

renderedIssues :: Fossa.Issues -> Doc ann
renderedIssues issues = rendered
  where
    issuesList :: [Fossa.Issue]
    issuesList = Fossa.issuesIssues issues

    categorize :: Ord k => (v -> k) -> [v] -> Map k [v]
    categorize f = M.fromListWith (++) . map (\v -> (f v, [v]))

    issuesByType :: Map Fossa.IssueType [Fossa.Issue]
    issuesByType = categorize Fossa.issueType issuesList

    renderSingle :: Fossa.IssueType -> [Fossa.Issue] -> Doc ann
    renderSingle ty rawIssues =
      renderHeader ty <> line <> vsep (map renderIssue rawIssues)

    rendered :: Doc ann
    rendered = vsep
      [renderSingle ty rawIssues | (ty,rawIssues) <- M.toList issuesByType]

    renderHeader ty = vsep
      [ "========================================================================"
      , pretty $ Fossa.renderIssueType ty
      , "========================================================================"
      , hsep $ map (fill 20) $ case ty of
          Fossa.IssuePolicyConflict -> ["Dependency", "Revision", "License"]
          Fossa.IssuePolicyFlag -> ["Dependency", "Revision", "License"]
          _ -> ["Dependency", "Revision"]
      , ""
      ]

    renderIssue :: Fossa.Issue -> Doc ann
    renderIssue issue = hsep (map format [name, revision, fromMaybe "" (Fossa.ruleLicenseId =<< Fossa.issueRule issue)])
      where
        format :: Text -> Doc ann
        format = fill 15 . pretty

        locatorSplit = T.split (\c -> c == '$' || c == '+') (Fossa.issueRevisionId issue)
        name = fromMaybe (Fossa.issueRevisionId issue) (locatorSplit !? 1)
        revision = fromMaybe "" (locatorSplit !? 2)

        (!?) :: [a] -> Int -> Maybe a
        xs !? ix
          | length xs <= ix = Nothing
          | otherwise = Just (xs Unsafe.!! ix)

data TestError
  = TestErrorAPI Fossa.FossaError
  | NotFinished Fossa.BuildStatus
  | NonEmptyIssues
  deriving (Show, Generic)

renderTestError :: TestError -> Text
renderTestError (TestErrorAPI err) = "An API error occurred: " <> T.pack (show err)
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
    Left err -> throwError (TestErrorAPI err)
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
    Left err -> throwError (TestErrorAPI err)
    Right issues ->
      case Fossa.issuesStatus issues of
        "WAITING" -> waitForIssues key name revision
        _ -> pure issues

timeout
  :: Int -- ^ number of seconds before timeout
  -> IO a
  -> IO (Maybe a)
timeout seconds act = either id id <$> Async.race (Just <$> act) (threadDelay (seconds * 1_000_000) *> pure Nothing)
