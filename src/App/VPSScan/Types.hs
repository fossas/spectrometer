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
    targetPath :: FilePath
  , dependencies :: [DepsDependency]
  , firstDependency :: Maybe DepsDependency
  , targetComponentName :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

data DepsDependency = DepsDependency
  { dependencyPath :: FilePath
  , dependencyComponentName :: Maybe Text
  , hasDependencies :: Bool
  } deriving (Eq, Ord, Show, Generic)

data NinjaGraphOpts = NinjaGraphOpts
  { ninjaGraphNinjaPath :: FilePath } deriving (Generic)
