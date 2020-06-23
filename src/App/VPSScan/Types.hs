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
  , depsGraphScotlandYardUrl :: UrlOption
  } deriving Generic
