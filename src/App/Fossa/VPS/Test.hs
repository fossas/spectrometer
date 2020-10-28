{-# LANGUAGE NumericUnderscores #-}

module App.Fossa.VPS.Test
  ( testMain,
    TestOutputType (..),
  )
where

import App.Fossa.API.BuildWait
import App.Fossa.VPS.EmbeddedBinary
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
import Path
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

data TestOutputType
  = -- | pretty output format for issues
    TestOutputPretty
  | -- | use json output for issues
    TestOutputJson

sherlockMonitorCmd ::
  -- | path to sherlock-cli binary
  Path Abs File ->
  VPSCore.SherlockInfo ->
  -- | scan ID
  Text ->
  Command
sherlockMonitorCmd sherlockCliPath sherlockInfo scanId =
  Command
    { cmdName = T.pack (fromAbsFile sherlockCliPath),
      cmdArgs =
        [ scanId,
          "-sherlock-api-host",
          VPSCore.sherlockUrl sherlockInfo,
          "-sherlock-api-client-id",
          VPSCore.sherlockClientId sherlockInfo,
          "-sherlock-api-secret-key",
          VPSCore.sherlockClientToken sherlockInfo
        ],
      cmdAllowErr = Never
    }

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
    result <- runDiagnostics $ withEmbeddedBinaries $ \binaries -> do
      revision <- mergeOverride override <$> inferProject (unBaseDir basedir)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")

      logSticky "[ Getting latest scan ID ]"

      Fossa.Organization orgId <- Fossa.getOrganization apiOpts
      ScotlandYard.LatestScanResponse scanId <- ScotlandYard.getLatestScan apiOpts (VPSCore.createLocator (projectName revision) orgId)
      sherlockInfo <- VPSCore.getSherlockInfo apiOpts

      logSticky "[ Waiting for component scan... ]"

      -- FIXME: this should probably call out to the real API
      -- we may not be able to count on stability for the API, though, which is
      -- why we use this command instead
      _ <- execThrow (unBaseDir basedir) (sherlockMonitorCmd (sherlockBinaryPath binaries) sherlockInfo scanId)

      logSticky "[ Waiting for build completion... ]"

      waitForBuild apiOpts revision

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
