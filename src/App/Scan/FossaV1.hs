module App.Scan.FossaV1
  ( uploadAnalysis
  ) where

import Data.List (isInfixOf)
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
  let filteredClosures = filter (isProductionPath . closureModuleDir) closures
      sourceUnits = map toSourceUnit filteredClosures
      opts = "locator" =: (renderLocator (Locator "custom" name (Just revision)))
          <> "v" =: cliVersion
          <> "managedBuild" =: True
          <> "title" =: name
          <> header "Authorization" ("token " <> encodeUtf8 key)
  resp <- runReq defaultHttpConfig $ req POST uploadUrl (ReqBodyJson sourceUnits) bsResponse opts
  logInfo ("Response from core: " <> (pretty $ decodeUtf8 $ responseBody resp))

-- we specifically want Rel paths here: parent directories shouldn't affect path
-- filtering
isProductionPath :: Path Rel fd -> Bool
isProductionPath path = not $ any (`isInfixOf` toFilePath path)
  [ "doc/"
  , "docs/"
  , "test/"
  , "example/"
  , "examples/"
  , "vendor/"
  , "node_modules/"
  , ".srclib-cache/"
  , "spec/"
  , "Godeps/"
  , ".git/"
  , "bower_components/"
  , "third_party/"
  , "third-party/"
  , "tmp/"
  , "Carthage/"
  , "Checkouts/"
  ]
