module App.Scan.FossaV1
  ( uploadAnalysis
  ) where

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effect.Logger
import Network.HTTP.Req
import Prologue
import Srclib.Converter (toSourceUnit)
import Srclib.Types
import Types

-- TODO: git commit?
cliVersion :: Text
cliVersion = "spectrometer"

uploadUrl :: Url 'Https
uploadUrl = https "app.fossa.com" /: "api" /: "builds" /: "custom"

-- TODO: IOExceptions?
-- TODO: filter project closures in test/vendor/etc directories
uploadAnalysis ::
  ( Has Logger sig m
  , MonadIO m
  )
  => Text -- api key
  -> Text -- project name
  -> Text -- project revision
  -> [ProjectClosure]
  -> m ()
uploadAnalysis key name revision closures = do
  let sourceUnits = map toSourceUnit closures
      opts = "locator" =: (renderLocator (Locator "custom" name (Just revision)))
          <> "v" =: cliVersion
          <> "managedBuild" =: True
          <> "title" =: name
          <> header "Authorization" ("token " <> encodeUtf8 key)
  resp <- runReq defaultHttpConfig $ req POST uploadUrl (ReqBodyJson sourceUnits) bsResponse opts
  logInfo ("Response from core: " <> (pretty $ decodeUtf8 $ responseBody resp))
