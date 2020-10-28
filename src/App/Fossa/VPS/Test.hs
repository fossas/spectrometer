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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
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
            TestOutputPretty -> logError (renderedIssues issues)
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

renderedIssues :: Fossa.Issues -> Doc ann
renderedIssues issues = rendered
  where
    padding :: Int
    padding = 20

    issuesList :: [Fossa.Issue]
    issuesList = Fossa.issuesIssues issues

    categorize :: Ord k => (v -> k) -> [v] -> Map k [v]
    categorize f = M.fromListWith (++) . map (\v -> (f v, [v]))

    issuesByType :: Map Fossa.IssueType [Fossa.Issue]
    issuesByType = categorize Fossa.issueType issuesList

    renderSection :: Fossa.IssueType -> [Fossa.Issue] -> Doc ann
    renderSection issueType rawIssues =
      renderHeader issueType <> line <> vsep (map renderIssue rawIssues) <> line

    rendered :: Doc ann
    rendered =
      vsep
        [renderSection issueType rawIssues | (issueType, rawIssues) <- M.toList issuesByType]

    renderHeader :: Fossa.IssueType -> Doc ann
    renderHeader ty =
      vsep
        [ "========================================================================",
          pretty $ Fossa.renderIssueType ty,
          "========================================================================",
          hsep $
            map (fill padding) $ case ty of
              Fossa.IssuePolicyConflict -> ["Dependency", "Revision", "License"]
              Fossa.IssuePolicyFlag -> ["Dependency", "Revision", "License"]
              _ -> ["Dependency", "Revision"],
          ""
        ]

    renderIssue :: Fossa.Issue -> Doc ann
    renderIssue issue = hsep (map format [name, revision, license])
      where
        format :: Text -> Doc ann
        format = fill padding . pretty

        locatorSplit = T.split (\c -> c == '$' || c == '+') (Fossa.issueRevisionId issue)

        name = fromMaybe (Fossa.issueRevisionId issue) (locatorSplit !? 1)
        revision = fromMaybe "" (locatorSplit !? 2)
        license = fromMaybe "" (Fossa.ruleLicenseId =<< Fossa.issueRule issue)

        (!?) :: [a] -> Int -> Maybe a
        xs !? ix
          | length xs <= ix = Nothing
          | otherwise = Just (xs !! ix)

timeout ::
  -- | number of seconds before timeout
  Int ->
  IO a ->
  IO (Maybe a)
timeout seconds act = either id id <$> Async.race (Just <$> act) (threadDelay (seconds * 1_000_000) $> Nothing)
