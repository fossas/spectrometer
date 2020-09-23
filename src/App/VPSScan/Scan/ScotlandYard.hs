{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module App.VPSScan.Scan.ScotlandYard
  ( createScotlandYardScan
  , uploadIPRResults
  , uploadBuildGraph
  , ScanResponse (..)
  , ScotlandYardOpts (..)
  , ScotlandYardNinjaOpts (..)
  )
where

import qualified Data.HashMap.Strict as HM
import Control.Carrier.TaskPool
import Control.Carrier.Diagnostics
import Control.Monad.IO.Class
import Control.Effect.Lift
import App.VPSScan.Types
import Data.Foldable (traverse_)
import Data.List.Split
import Effect.Logger
import GHC.Conc.Sync (getNumCapabilities)
import Data.Maybe
import qualified Data.Vector as V
import App.VPSScan.Scan.Core
import Control.Carrier.Trace.Printing
import Data.Aeson
import Data.Text (Text, unpack)
import Network.HTTP.Req
import App.Util (parseUri)

data ScotlandYardOpts = ScotlandYardOpts
  { projectId :: Locator
  , projectRevision :: Text
  , organizationId :: Int
  , syVpsOpts :: VPSOpts
  }

data ScotlandYardNinjaOpts = ScotlandYardNinjaOpts
  { syNinjaProjectId :: Locator
  , syNinjaOrganizationId :: Int
  , syNinjaOpts :: NinjaGraphOpts
  }

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

-- /projects/{projectID}/scans
createScanEndpoint :: Url 'Https -> Text -> Url 'Https
createScanEndpoint baseurl projectId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans"

-- /projects/{projectID}/scans/{scanID}/discovered_licenses
scanDataEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
scanDataEndpoint baseurl projectId scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "discovered_licenses"

data ScanResponse = ScanResponse
  { responseScanId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse <$> obj .: "scanId"

data CreateBuildGraphResponse = CreateBuildGraphResponse
  { responseBuildGraphId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CreateBuildGraphResponse where
  parseJSON = withObject "CreateBuildGraphResponse" $ \obj ->
    CreateBuildGraphResponse <$> obj .: "id"

createScotlandYardScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ScotlandYardOpts -> m ScanResponse
createScotlandYardScan ScotlandYardOpts {..} = runHTTP $ do
  let VPSOpts{..} = syVpsOpts
  let FossaOpts{..} = fossa

  let body = object ["revisionId" .= projectRevision, "organizationId" .= organizationId]
  let auth = coreAuthHeader fossaApiKey
  let locator = unLocator projectId

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req POST (createScanEndpoint baseUrl locator) (ReqBodyJson body) jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IPR result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
uploadIPRResults :: (ToJSON a, Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> a -> ScotlandYardOpts -> m ()
uploadIPRResults scanId value ScotlandYardOpts {..} = runHTTP $ do
  let VPSOpts{..} = syVpsOpts
  let FossaOpts{..} = fossa
  let auth = coreAuthHeader fossaApiKey
  let locator = unLocator projectId

  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (scanDataEndpoint baseUrl locator scanId) (ReqBodyJson value) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()

-- /projects/{projectID}/scans/{scanID}/build-graphs
createBuildGraphEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
createBuildGraphEndpoint baseurl projectId scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "build-graphs"

-- /projects/{projectID}/scans/{scanID}/build-graphs/{buildGraphID}/rules
uploadBuildGraphChunkEndpoint :: Url 'Https -> Text -> Text -> Text ->  Url 'Https
uploadBuildGraphChunkEndpoint baseurl projectId scanId buildGraphId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "build-graphs" /: buildGraphId /: "rules"

-- /projects/{projectID}/scans/{scanID}/build-graphs/{buildGraphID}/rules/complete
uploadBuildGraphCompleteEndpoint :: Url 'Https -> Text -> Text -> Text ->  Url 'Https
uploadBuildGraphCompleteEndpoint baseurl projectId scanId buildGraphId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "build-graphs" /: buildGraphId /: "rules" /: "complete"

-- create the build graph in SY, upload it in chunks and then complete it.
uploadBuildGraph :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ScotlandYardNinjaOpts -> [DepsTarget] -> m ()
uploadBuildGraph syOpts@ScotlandYardNinjaOpts {..} graph = runHTTP $ do
  let NinjaGraphOpts{..} = syNinjaOpts
  let FossaOpts{..} = ninjaFossaOpts
  let auth = coreAuthHeader fossaApiKey
  let locator = unLocator syNinjaProjectId
  -- let body = object ["display_name" .= buildName]
  (baseUrl, baseOptions) <- parseUri fossaUrl
  runTrace $ trace "creating build graph..."
  buildGraphId <- createBuildGraph syOpts
  -- resp <- req POST (createBuildGraphEndpoint baseUrl locator scanId) (ReqBodyJson body) jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  -- runTrace $ trace "build graph created"
  -- let buildGraphId = CreateBuildGraphResponse $ responseBody resp
  runTrace $ trace "new build graph id "
  runTrace $ trace $ unpack $ responseBuildGraphId buildGraphId
  let body = object ["targets" .= graph]
  let chunkedJSON = fromMaybe [] (chunkJSON body "targets" 1000)

  capabilities <- liftIO getNumCapabilities
  let url = uploadBuildGraphChunkEndpoint baseUrl locator scanId (responseBuildGraphId buildGraphId)
  _ <- liftIO $ withLogger SevTrace $ withTaskPool capabilities updateProgress $ traverse_ (forkTask . uploadBuildGraphChunk url baseOptions) chunkedJSON
  -- _ <- req POST (uploadBuildGraphChunkEndpoint baseUrl locator scanId (responseBuildGraphId buildGraphId)) (ReqBodyJson body) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  runTrace $ trace "chunks uploaded"
  _ <- req PUT (uploadBuildGraphCompleteEndpoint baseUrl locator scanId (responseBuildGraphId buildGraphId)) (ReqBodyJson $ object []) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  runTrace $ trace "completed"
  pure ()

createBuildGraph :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ScotlandYardNinjaOpts -> m CreateBuildGraphResponse
createBuildGraph ScotlandYardNinjaOpts {..} = runHTTP $ do
  let NinjaGraphOpts{..} = syNinjaOpts
  let FossaOpts{..} = ninjaFossaOpts
  let auth = coreAuthHeader fossaApiKey
  let locator = unLocator syNinjaProjectId
  let body = object ["display_name" .= buildName]
  (baseUrl, baseOptions) <- parseUri fossaUrl
  runTrace $ trace "creating build graph..."
  resp <- req POST (createBuildGraphEndpoint baseUrl locator scanId) (ReqBodyJson body) jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  runTrace $ trace "build graph created"
  pure (responseBody resp)

uploadBuildGraphChunk :: (Has (Lift IO) sig m) => Url 'Https -> Option 'Https -> Value -> m ()
uploadBuildGraphChunk url postOptions jsonChunk = do
  _ <- sendIO $ runDiagnostics $ runHTTP $ req POST url (ReqBodyJson jsonChunk) ignoreResponse (postOptions <> header "Content-Type" "application/json")
  pure ()

updateProgress :: Has Logger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )

chunkJSON :: Value -> Text -> Int -> Maybe [Value]
chunkJSON (Object obj) key chunkSize = do
  a <- HM.lookup key obj
  arr <- case a of
    Array aa -> Just aa
    _ -> Nothing
  let chunked = chunksOf chunkSize $ V.toList arr
  let chunker :: [Value] -> Value
      chunker v = object ["Files" .= v]
  Just $ map chunker chunked

chunkJSON _ _ _ = Nothing