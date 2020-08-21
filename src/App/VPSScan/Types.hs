module App.VPSScan.Types
( VPSOpts(..)
, SherlockOpts(..)
, FossaOpts(..)
, DepsTarget(..)
, DepsDependency(..)
, NinjaGraphOpts(..)
, runHTTP
, HTTP(..)
, HTTPRequestFailed(..)
) where

import qualified App.VPSScan.Scan.RunIPR as RunIPR
import Control.Carrier.Diagnostics
import Prologue
import Text.URI (URI)
import Network.HTTP.Req
import Data.Text.Prettyprint.Doc (viaShow)

data FossaOpts = FossaOpts
  {fossaUrl :: URI, fossaApiKey :: Text}
  deriving (Generic)

data SherlockOpts = SherlockOpts
  { sherlockCmdPath :: Text,
    sherlockUrl :: Text,
    sherlockClientToken :: Text,
    sherlockClientID :: Text
  }
  deriving (Eq, Ord, Show, Generic)

data VPSOpts = VPSOpts
  { vpsSherlock :: SherlockOpts
  , vpsIpr :: Maybe RunIPR.IPROpts
  , fossaInstance :: FossaOpts
  , organizationID :: Int
  , projectID :: Text
  , revisionID :: Text
  , filterExpressions :: RunIPR.FilterExpressions
  } deriving (Generic)

data DepsTarget = DepsTarget
  {
    targetPath :: Text
  , targetDependencies :: [DepsDependency]
  , targetInputs :: [DepsDependency]
  , targetComponentName :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON DepsTarget where
  toJSON DepsTarget{..} = object
    [ "path" .= targetPath
    , "dependencies" .= targetDependencies
    , "inputs" .= targetInputs
    , "component_name" .=  targetComponentName
    ]

data DepsDependency = DepsDependency
  { dependencyPath :: Text
  , dependencyComponentName :: Maybe Text
  , isTarget :: Bool
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON DepsDependency where
  toJSON DepsDependency{..} = object
    [ "path" .= dependencyPath
    , "component_name" .= dependencyComponentName
    , "is_target" .= isTarget
    ]

data NinjaGraphOpts = NinjaGraphOpts
  { ninjaGraphNinjaPath :: Maybe FilePath
  , lunchTarget :: Maybe Text
  , depsGraphFossaUrl :: URI
  , depsGraphProjectID :: Text
  , depsGraphScanID :: Text
  , depsGraphBuildName :: Text
  } deriving Generic

newtype HTTP m a = HTTP {unHTTP :: m a}
  deriving (Functor, Applicative, Monad, MonadIO, Algebra sig)

data HTTPRequestFailed = HTTPRequestFailed HttpException
  deriving (Show)

instance ToDiagnostic HTTPRequestFailed where
  renderDiagnostic (HTTPRequestFailed exc) = "An HTTP request failed: " <> viaShow exc

instance (MonadIO m, Has Diagnostics sig m) => MonadHttp (HTTP m) where
  handleHttpException = HTTP . fatal . HTTPRequestFailed

runHTTP :: HTTP m a -> m a
runHTTP = unHTTP
