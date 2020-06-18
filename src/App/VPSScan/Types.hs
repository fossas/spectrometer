module App.VPSScan.Types
( VPSOpts(..)
, SherlockOpts(..)
, ScotlandYardOpts(..)
, DepsTarget(..)
, DepsDependency(..)
, NinjaGraphOpts(..)
) where

import qualified App.VPSScan.Scan.RunIPR as RunIPR
import Prologue
import Text.URI (URI)

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

data VPSOpts = VPSOpts
  { vpsSherlock :: SherlockOpts
  , vpsIpr :: Maybe RunIPR.IPROpts
  , vpsScotlandYard :: ScotlandYardOpts
  , organizationID :: Int
  , projectID :: Text
  , revisionID :: Text
  } deriving (Generic)

data DepsTarget = DepsTarget
  {
    targetPath :: ByteString
  , dependencies :: [DepsDependency]
  , inputs :: [DepsDependency]
  , targetComponentName :: Maybe ByteString
  } deriving (Eq, Ord, Show, Generic)

data DepsDependency = DepsDependency
  { dependencyPath :: ByteString
  , dependencyComponentName :: Maybe ByteString
  , hasDependencies :: Bool
  } deriving (Eq, Ord, Show, Generic)

data NinjaGraphOpts = NinjaGraphOpts
  { ninjaGraphNinjaPath :: Maybe FilePath
  , lunchTarget :: Maybe Text
  } deriving Generic
