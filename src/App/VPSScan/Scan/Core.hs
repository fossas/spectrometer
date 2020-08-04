module App.VPSScan.Scan.Core
  ( coreAuthHeader
  , createCoreProject
  , getSherlockInfo
  , createLocator
  , SherlockInfo(..)
  )
where

import App.VPSScan.Types
import App.Util (parseUri)
import Data.Text (Text)
import Prelude
import Network.HTTP.Req
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.IO.Class (MonadIO)
import Control.Effect.Diagnostics
import Data.Aeson

data SherlockInfo = SherlockInfo
  { sherlockUrl :: Text
  , sherlockClientToken :: Text
  , sherlockClientId :: Text
  , sherlockOrgId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON SherlockInfo where
  parseJSON = withObject "" $ \obj -> do
    auth <- obj .: "auth"
    sherlockUrl <- obj .: "url"
    sherlockOrgId <- obj .: "orgId"
    sherlockClientId <- auth .: "clientId"
    sherlockClientToken <- auth .: "clientToken"
    return (SherlockInfo { sherlockUrl = sherlockUrl, sherlockClientId = sherlockClientId, sherlockClientToken = sherlockClientToken, sherlockOrgId = sherlockOrgId })

coreAuthHeader :: Text -> Option scheme
coreAuthHeader apiKey = header "Authorization" (encodeUtf8 ("Bearer " <> apiKey))

createLocator :: Text -> Text -> Text
createLocator projectName organizationId = "custom+" <> organizationId <> "/" <> projectName

-- /api/vendored-package-scan/sherlock-info
sherlockInfoEndpoint :: Url 'Https -> Url 'Https
sherlockInfoEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "sherlock-info"

-- /api/vendored-package-scan/ci
createProjectEndpoint :: Url 'Https -> Url 'Https
createProjectEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "ci"

createCoreProject :: (MonadIO m, Has Diagnostics sig m) => Text -> Text -> FossaOpts -> m ()
createCoreProject name revision FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey
  let body = object ["name" .= name, "revision" .= revision]

  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (createProjectEndpoint baseUrl) (ReqBodyJson body) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()

getSherlockInfo :: (MonadIO m, Has Diagnostics sig m) => FossaOpts -> m SherlockInfo
getSherlockInfo FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req GET (sherlockInfoEndpoint baseUrl) NoReqBody jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)
