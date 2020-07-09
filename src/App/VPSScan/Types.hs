module App.VPSScan.Types
( VPSOpts(..)
, SherlockOpts(..)
, ScotlandYardOpts(..)
, DepsTarget(..)
, DepsDependency(..)
, IPROpts(..)
, NinjaGraphOpts(..)
, runHTTP
, HTTP(..)
, HTTPRequestFailed(..)
) where

import Control.Carrier.Diagnostics
import Prologue
import Text.URI (URI)
import Network.HTTP.Req
import Data.Text.Prettyprint.Doc (viaShow)

data ScotlandYardOpts = ScotlandYardOpts
  {scotlandYardUrl :: URI}
  deriving (Generic)

data SherlockOpts = SherlockOpts
  { sherlockCmdPath :: String,
    sherlockUrl :: String,
    sherlockClientToken :: String,
    sherlockClientID :: String
  }
  deriving (Eq, Ord, Show, Generic)

data IPROpts = IPROpts
  { iprCmdPath :: String
  , nomosCmdPath :: String
  , pathfinderCmdPath :: String
  , s3Bucket :: Maybe String
  , s3Endpoint :: Maybe URI
  }
  deriving (Eq, Ord, Show)

data VPSOpts = VPSOpts
  { vpsSherlock :: SherlockOpts
  , vpsIpr :: Maybe IPROpts
  , vpsScotlandYard :: ScotlandYardOpts
  , organizationID :: Int
  , projectID :: Text
  , revisionID :: Text
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
    , "componentName" .=  targetComponentName
    ]

data DepsDependency = DepsDependency
  { dependencyPath :: Text
  , dependencyComponentName :: Maybe Text
  , hasDependencies :: Bool
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON DepsDependency where
  toJSON DepsDependency{..} = object
    [ "path" .= dependencyPath
    , "componentName" .= dependencyComponentName
    , "hasDependencies" .= hasDependencies
    ]

data NinjaGraphOpts = NinjaGraphOpts
  { ninjaGraphNinjaPath :: Maybe FilePath
  , lunchTarget :: Maybe Text
  , depsGraphScotlandYardUrl :: URI
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
