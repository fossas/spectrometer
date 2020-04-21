module VPSScan.ScotlandYard
  ( createScan
  , postIprResults
  , ScotlandYardOpts(..)
  , HTTP(..)
  , runHTTP
  )
where
import Prologue

import Control.Carrier.Error.Either
import Network.HTTP.Req

data ScotlandYardOpts = ScotlandYardOpts
  { scotlandYardUrl :: Url 'Https
  , organizationID :: Text
  , projectID :: Text
  , revisionID :: Text
  } deriving (Eq, Ord, Show, Generic)

newtype HTTP a = HTTP { unHTTP :: ErrorC HttpException IO a }
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
  { scanId :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse <$> obj .: "scanId"

createScan :: ScotlandYardOpts -> HTTP ScanResponse
createScan ScotlandYardOpts{..} = do
  let body = object ["organizationId" .= organizationID, "revisionId" .= revisionID]
  resp <- req POST (createScanEndpoint scotlandYardUrl projectID) (ReqBodyJson body) jsonResponse mempty
  pure (responseBody resp)

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IRP result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
postIprResults :: ToJSON a => ScotlandYardOpts -> Text -> a -> HTTP ()
postIprResults ScotlandYardOpts{..} scanId value = do
  req POST (scanDataEndpoint scotlandYardUrl projectID scanId) (ReqBodyJson value) ignoreResponse mempty
  pure ()
