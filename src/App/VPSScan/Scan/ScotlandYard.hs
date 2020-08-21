module App.VPSScan.Scan.ScotlandYard
  ( HTTP (..),
    runHTTP,
    ScanResponse (..),
    CreateDepsGraphResponse (..),
    createDependencyGraph,
    uploadDependencyGraphData,
    markDependencyGraphComplete,
    createScotlandYardScan,
    uploadIPRResults,
  )
where

import App.VPSScan.Types
import Control.Carrier.TaskPool
import Control.Carrier.Diagnostics
import Control.Effect.Lift
import Data.List.Split
import GHC.Conc.Sync (getNumCapabilities)
import Effect.Logger
import Data.Aeson
import Network.HTTP.Req
import Prologue
import App.Util (parseUri)
import Data.Text.Encoding

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

authHeader :: Text -> Option scheme
authHeader apiKey = header "Authorization" (encodeUtf8 ("Bearer " <> apiKey))

-- /projects/{projectID}/scans
createScanEndpoint :: Url 'Https -> Text -> Url 'Https
createScanEndpoint baseurl projectId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans"

-- /projects/{projectID}/scans/{scanID}/discovered_licenses
scanDataEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
scanDataEndpoint baseurl projectId scanId = baseurl /: "projects" /: projectId /: "scans" /: scanId /: "discovered_licenses"

-- /projects/{projectID}/scans/{scanID}/dependency_graphs
createDependencyGraphEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
createDependencyGraphEndpoint baseurl projectId scanId = baseurl /: "projects" /: projectId /: "scans" /: scanId /: "dependency_graphs"

-- /projects/{projectID}/scans/{scanID}/dependency_graphs/{depsGraphID}/rules
uploadDependencyGraphDataEndpoint :: Url 'Https -> Text -> Text -> Text -> Url 'Https
uploadDependencyGraphDataEndpoint baseurl projectId scanId depsGraphID = baseurl /: "projects" /: projectId /: "scans" /: scanId /: "dependency_graphs" /: depsGraphID /: "rules"

-- /projects/{projectID}/scans/{scanID}/dependency_graphs/{depsGraphID}/rules/complete
markDependencyGraphCompleteEndpoint :: Url 'Https -> Text -> Text -> Text -> Url 'Https
markDependencyGraphCompleteEndpoint baseurl projectId scanId depsGraphID = baseurl /: "projects" /: projectId /: "scans" /: scanId /: "dependency_graphs" /: depsGraphID /: "rules" /: "complete"

data ScanResponse = ScanResponse
  { responseScanId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse <$> obj .: "scanId"

data CreateDepsGraphResponse = CreateDepsGraphResponse
  { responseDepsGraphID :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CreateDepsGraphResponse where
  parseJSON = withObject "CreateDepsGraphResponse" $ \obj ->
    CreateDepsGraphResponse <$> obj .: "dependency_graph_id"

createScotlandYardScan :: (MonadIO m, Has Diagnostics sig m) => VPSOpts -> m ScanResponse
createScotlandYardScan VPSOpts {..} = runHTTP $ do
  let body = object ["organizationId" .= organizationID, "revisionId" .= revisionID, "projectId" .= projectID]; FossaOpts {..} = fossaInstance
  let auth = authHeader fossaApiKey

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req POST (createScanEndpoint baseUrl projectID) (ReqBodyJson body) jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IPR result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
uploadIPRResults :: (ToJSON a, MonadIO m, Has Diagnostics sig m) => VPSOpts -> Text -> a -> m ()
uploadIPRResults VPSOpts {..} scanId value = runHTTP $ do
  let FossaOpts {..} = fossaInstance
  let auth = authHeader fossaApiKey

  (baseUrl, baseOptions) <- parseUri fossaUrl

  _ <- req POST (scanDataEndpoint baseUrl projectID scanId) (ReqBodyJson value) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()

createDependencyGraph :: (MonadIO m, Has Diagnostics sig m) => NinjaGraphOpts -> m CreateDepsGraphResponse
createDependencyGraph NinjaGraphOpts{..} = runHTTP $ do
  (baseUrl, baseOptions) <- parseUri depsGraphFossaUrl
  resp <- req POST (createDependencyGraphEndpoint baseUrl depsGraphProjectID depsGraphScanID) (ReqBodyJson (object ["graphName" .= depsGraphBuildName])) jsonResponse (baseOptions <> header "Content-Type" "application/json" <> header "Fossa-Org-Id" "1")
  pure (responseBody resp)

uploadDependencyGraphData :: (MonadIO m, Has Diagnostics sig m) => NinjaGraphOpts -> Text -> [DepsTarget] -> m ()
uploadDependencyGraphData NinjaGraphOpts{..} depsGraphID graph = runHTTP $ do
  (baseUrl, baseOptions) <- parseUri depsGraphFossaUrl
  let url = uploadDependencyGraphDataEndpoint baseUrl depsGraphProjectID depsGraphScanID depsGraphID
  let chunkedGraph = chunksOf 1000 graph
  capabilities <- liftIO getNumCapabilities
  _ <- liftIO $ withLogger SevTrace $ withTaskPool capabilities updateProgress $ traverse_ (forkTask . uploadDependencyGraphChunk url baseOptions) chunkedGraph
  pure ()

uploadDependencyGraphChunk :: (Has (Lift IO) sig m) => Url 'Https -> Option 'Https -> [DepsTarget] -> m ()
uploadDependencyGraphChunk url postOptions depsGraphChunk = do
  _ <- sendIO $ runDiagnostics $ runHTTP $ req POST url (ReqBodyJson (object ["targets" .= depsGraphChunk])) ignoreResponse (postOptions <> header "Content-Type" "application/json")
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

markDependencyGraphComplete :: (MonadIO m, Has Diagnostics sig m) => NinjaGraphOpts -> Text -> m ()
markDependencyGraphComplete NinjaGraphOpts{..} depsGraphID = runHTTP $ do
  (baseUrl, baseOptions) <- parseUri depsGraphFossaUrl
  _ <- req PUT (markDependencyGraphCompleteEndpoint baseUrl depsGraphProjectID depsGraphScanID depsGraphID) (ReqBodyJson ()) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> header "Fossa-Org-Id" "1")
  pure ()

