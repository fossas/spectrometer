{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.DocsSpec (
  spec,
) where

import App.Docs (newIssueUrl, userGuideUrl)
import Control.Exception (throwIO)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.HTTP.Req (
  GET (GET),
  MonadHttp (handleHttpException),
  NoReqBody (NoReqBody),
  bsResponse,
  req,
  responseStatusCode,
  useHttpsURI,
 )
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Text.URI (mkURI)

instance MonadHttp IO where
  handleHttpException = throwIO

shouldRespondToGETWithHttpCode :: Text -> Int -> Expectation
shouldRespondToGETWithHttpCode uri expected = do
  (url, _) <- fromJust . useHttpsURI <$> mkURI uri
  r <- req GET url NoReqBody bsResponse mempty
  responseStatusCode r `shouldBe` expected

spec :: Spec
spec = do
  describe "userGuideUrl" $
    it "should be reachable" $
      userGuideUrl `shouldRespondToGETWithHttpCode` 200

  describe "newIssueUrl" $
    it "should be reachable" $
      newIssueUrl `shouldRespondToGETWithHttpCode` 200
