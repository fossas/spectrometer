{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.VPS.Types
( FilterExpressions(..)
, encodeFilterExpressions
, HTTP(..)
, runHTTP
, HTTPRequestFailed(..)
, NinjaGraphOptions(..)
) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Carrier.Diagnostics
import Control.Effect.Lift (Lift, sendIO)
import Data.Text (Text)
import Data.Aeson
import Fossa.API.Types (ApiOpts)
import Network.HTTP.Req
import Data.Text.Prettyprint.Doc (viaShow)
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8)
import Effect.Logger (Severity)
import App.Types (BaseDir, OverrideProject(..))

newtype FilterExpressions = FilterExpressions { unFilterExpressions :: [Text] }

encodeFilterExpressions :: FilterExpressions -> Text
encodeFilterExpressions filters = decodeUtf8 $ BSL.toStrict $ encode (unFilterExpressions filters)

instance FromJSON FilterExpressions where
  parseJSON = withObject "CoreFilterExpressions" $ \obj -> FilterExpressions <$> obj .: "filters"

instance ToJSON FilterExpressions where
  toJSON (FilterExpressions filters) = object ["filters" .= filters]

newtype HTTP m a = HTTP {unHTTP :: m a}
  deriving (Functor, Applicative, Monad, Algebra sig)

instance Has (Lift IO) sig m => MonadIO (HTTP m) where
  liftIO = sendIO

newtype HTTPRequestFailed = HTTPRequestFailed HttpException deriving (Show)

instance ToDiagnostic HTTPRequestFailed where
  renderDiagnostic (HTTPRequestFailed exc) = "An HTTP request failed: " <> viaShow exc

instance (Has (Lift IO) sig m, Has Diagnostics sig m) => MonadHttp (HTTP m) where
  handleHttpException = HTTP . fatal . HTTPRequestFailed

runHTTP :: HTTP m a -> m a
runHTTP = unHTTP

data NinjaGraphOptions = NinjaGraphOptions
  { -- TODO: These three fields seem fairly common. Factor out into `CommandOptions t`?
    ngoLogSeverity :: Severity,
    ngoAPIOptions :: ApiOpts,
    ngoProjectOverride :: OverrideProject,
    --
    ngoAndroidTopDir :: BaseDir,
    ngoLunchCombo :: Text,
    ngoScanID :: Text,
    ngoBuildName :: Text
  }
