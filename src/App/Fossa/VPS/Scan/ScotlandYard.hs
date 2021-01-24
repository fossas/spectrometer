{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan.ScotlandYard
  ( uploadBuildGraph,
    getScan,
    getLatestScan,
    ScanResponse (..),
  )
where

import App.Fossa.VPS.Scan.Core (Locator (..))
import App.Fossa.VPS.Types (NinjaGraphOptions (..), runHTTP)
import Control.Carrier.Diagnostics (Diagnostics, runDiagnostics)
import Control.Carrier.TaskPool (Progress (..), forkTask, withTaskPool)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, encode, object, parseJSON, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (traverse_)
import Data.Text (Text)
import Effect.Logger (Color (..), Logger, Severity (..), annotate, color, logSticky, pretty, withLogger)
import Fossa.API.Types (ApiOpts, useApiOpts)
import GHC.Conc.Sync (getNumCapabilities)
import Network.HTTP.Req (GET (..), NoReqBody (..), Option, POST (..), PUT (..), ReqBodyJson (..), Scheme (..), Url, header, ignoreResponse, jsonResponse, req, responseBody, (/:), (=:))

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

getScanEndpoint :: Url 'Https -> Locator -> Text -> Url 'Https
getScanEndpoint baseurl (Locator projectId) scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId

getLatestScanEndpoint :: Url 'Https -> Locator -> Url 'Https
getLatestScanEndpoint baseurl (Locator projectId) = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: "latest"

newtype CreateScanResponse = CreateScanResponse
  { createScanResponseId :: Text
  }
  deriving (Eq, Ord, Show)

data ScanResponse = ScanResponse
  { responseScanId :: Text,
    responseScanStatus :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CreateScanResponse where
  parseJSON = withObject "CreateScanResponse" $ \obj ->
    CreateScanResponse
      <$> obj .: "scanId"

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse
      <$> obj .: "id"
      <*> obj .:? "status"

newtype CreateBuildGraphResponse = CreateBuildGraphResponse
  { responseBuildGraphId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CreateBuildGraphResponse where
  parseJSON = withObject "CreateBuildGraphResponse" $ \obj ->
    CreateBuildGraphResponse <$> obj .: "id"

getLatestScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> Locator -> Text -> m ScanResponse
getLatestScan apiOpts locator revisionId = runHTTP $ do
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  let opts =
        baseOptions
          <> header "Content-Type" "application/json"
          <> "revisionID" =: revisionId
  resp <- req GET (getLatestScanEndpoint baseUrl locator) NoReqBody jsonResponse opts
  pure (responseBody resp)

getScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> Locator -> Text -> m ScanResponse
getScan apiOpts locator scanId = runHTTP $ do
  (baseUrl, baseOptions) <- useApiOpts apiOpts
  let opts =
        baseOptions
          <> header "Content-Type" "application/json"
  resp <- req GET (getScanEndpoint baseUrl locator scanId) NoReqBody jsonResponse opts
  pure (responseBody resp)

-- Create a new build graph in SY, upload it in chunks of ~ 1 MB, and then complete it.
uploadBuildGraph :: (Has (Lift IO) sig m, Has Diagnostics sig m, ToJSON t) => NinjaGraphOptions -> Locator -> [t] -> m ()
uploadBuildGraph NinjaGraphOptions {..} (Locator locator) graph = runHTTP $ do
  (baseURL, baseOptions) <- useApiOpts ngoAPIOptions
  let options = baseOptions <> header "Content-Type" "application/json"

  -- Create the build graph and save its ID.
  CreateBuildGraphResponse {..} <- createBuildGraph baseURL options

  -- Split the build graph data into chunks, and upload them in parallel.
  capabilities <- sendIO getNumCapabilities
  _ <-
    sendIO $
      withLogger SevTrace $
        withTaskPool capabilities updateProgress $
          traverse_
            (forkTask . runDiagnostics . uploadBuildGraphChunk baseURL options responseBuildGraphId)
            chunkedGraph

  -- Mark the build graph as complete.
  markBuildComplete baseURL options responseBuildGraphId
  where
    chunkedGraph = chunkedBySize graph (1024 * 1024)

    createBuildGraph ::
      (Has (Lift IO) sig m, Has Diagnostics sig m) =>
      Url 'Https ->
      Option 'Https ->
      m CreateBuildGraphResponse
    createBuildGraph baseURL options = runHTTP $ do
      resp <- req POST url (ReqBodyJson body) jsonResponse options
      pure (responseBody resp)
      where
        url =
          coreProxyPrefix baseURL
            /: "projects"
            /: locator
            /: "scans"
            /: ngoScanID
            /: "build-graphs"
        body = object ["display_name" .= ngoBuildName]

    uploadBuildGraphChunk ::
      (Has (Lift IO) sig m, Has Diagnostics sig m, ToJSON t) =>
      Url 'Https ->
      Option 'Https ->
      Text ->
      [t] ->
      m ()
    uploadBuildGraphChunk baseURL options buildGraphID chunk =
      runHTTP $ void $ req POST url (ReqBodyJson jsonChunk) ignoreResponse options
      where
        jsonChunk = object ["targets" .= chunk]
        url =
          coreProxyPrefix baseURL
            /: "projects"
            /: locator
            /: "scans"
            /: ngoScanID
            /: "build-graphs"
            /: buildGraphID
            /: "rules"

    markBuildComplete ::
      (Has (Lift IO) sig m, Has Diagnostics sig m) =>
      Url 'Https ->
      Option 'Https ->
      Text ->
      m ()
    markBuildComplete baseURL options buildGraphID =
      runHTTP $ void $ req PUT url (ReqBodyJson $ object []) ignoreResponse options
      where
        url =
          coreProxyPrefix baseURL
            /: "projects"
            /: locator
            /: "scans"
            /: ngoScanID
            /: "build-graphs"
            /: buildGraphID
            /: "rules"
            /: "complete"

    -- TODO: There are a LOT of duplications of this -- refactor? Do CLI UI
    -- primitives in general need a refactor?
    updateProgress :: Has Logger sig m => Progress -> m ()
    updateProgress Progress {..} =
      logSticky
        ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]"
        )

    -- Partition list items by JSON encoding size, limiting partition encoding
    -- size to maxByteSize. Items that are themselves larger than maxByteSize
    -- may form one-element partitions that are larger than maxByteSize.
    chunkedBySize :: (ToJSON a) => [a] -> Int -> [[a]]
    chunkedBySize xs maxByteSize = chunked
      where
        chunked = snd $ foldr (addToList maxByteSize) (0, [[]]) xs

        addToList :: (ToJSON a) => Int -> a -> (Int, [[a]]) -> (Int, [[a]])
        addToList maxLength ele (currentLength, first : rest) =
          if (currentLength + newLength) > maxLength
            then (newLength, [ele] : first : rest)
            else (newLength + currentLength, (ele : first) : rest)
          where
            newLength = fromIntegral $ BS.length $ encode ele
        addToList _ ele (n, []) = (n, [[ele]])
