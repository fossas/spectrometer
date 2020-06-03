module App.VPSScan.Scan.ScotlandYard
  ( HTTP (..),
    runHTTP,
    ScanResponse (..),
    createScotlandYardScan,
    uploadIPRResults,
  )
where

import App.VPSScan.Types
import Control.Carrier.Error.Either
import Network.HTTP.Req
import OptionExtensions (UrlOption (..))
import Prologue

newtype HTTP a = HTTP {unHTTP :: ErrorC HttpException IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runHTTP :: MonadIO m => HTTP a -> m (Either HttpException a)
runHTTP = liftIO . runError @HttpException . unHTTP

instance MonadHttp HTTP where
  handleHttpException = HTTP . throwError

-- /projects/{projectID}/scans
createScanEndpoint :: Url 'Https -> Text -> Url 'Https
createScanEndpoint baseurl projectId = baseurl /: "projects" /: projectId /: "scans"

-- /projects/{projectID}/scans/{scanID}/discovered_licenses
scanDataEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
scanDataEndpoint baseurl projectId scanId = baseurl /: "projects" /: projectId /: "scans" /: scanId /: "discovered_licenses"

data ScanResponse = ScanResponse
  { responseScanId :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse <$> obj .: "scanId"

createScotlandYardScan :: MonadIO m => VPSOpts -> m (Either HttpException ScanResponse)
createScotlandYardScan VPSOpts {..} = runHTTP $ do
  let body = object ["organizationId" .= organizationID, "revisionId" .= revisionID, "projectId" .= projectID]
      ScotlandYardOpts {..} = vpsScotlandYard
  resp <- req POST (createScanEndpoint (urlOptionUrl scotlandYardUrl) projectID) (ReqBodyJson body) jsonResponse (urlOptionOptions scotlandYardUrl <> header "Content-Type" "application/json")
  pure (responseBody resp)

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IPR result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
uploadIPRResults :: (ToJSON a, MonadIO m) => VPSOpts -> Text -> a -> m (Either HttpException ())
uploadIPRResults VPSOpts {..} scanId value = runHTTP $ do
  let ScotlandYardOpts {..} = vpsScotlandYard
  _ <- req POST (scanDataEndpoint (urlOptionUrl scotlandYardUrl) projectID scanId) (ReqBodyJson value) ignoreResponse (urlOptionOptions scotlandYardUrl <> header "Content-Type" "application/json")
  pure ()
