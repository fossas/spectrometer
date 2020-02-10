module Strategy.NuGet.Nuspec
  ( discover
  , strategy
  , buildGraph
  , analyze

  , Nuspec(..)
  , Group(..)
  , NuGetDependency(..)
  , License(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Text as T
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           DepTypes
import           Discovery.Walk
import           Effect.ReadFS
import           Graphing (Graphing, unfold)
import           Parse.XML
import           Types

discover :: Discover
discover = Discover
  { discoverName = "nuspec"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> L.isSuffixOf ".nuspec" (fileName f)) files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nuget-nuspec"
  , strategyAnalyze = \opts -> analyze & fileInputXML @Nuspec (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

analyze :: Member (Input Nuspec) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

data Nuspec = Nuspec
  { groups        :: [Group]
  , license       :: Maybe License
  , licenseUrl    :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

data License = License
  { licenseType  :: Maybe Text
  , value        :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

newtype Group = Group
  { dependencies  :: [NuGetDependency]
  } deriving (Eq, Ord, Show, Generic)

data NuGetDependency = NuGetDependency
  { depID      :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromXML Nuspec where
  parseElement el = do
    metadata     <- child "metadata" el
    dependencies <- child "dependencies" metadata
    Nuspec <$> children "group" dependencies
           <*> optional (child "license" metadata)
           <*> optional (child "licenseUrl" metadata)

instance FromXML License where
  parseElement el =
    License <$> optional (attr "type" el)
            <*> optional (content el)

instance FromXML Group where
  parseElement el = Group <$> children "dependency" el

instance FromXML NuGetDependency where
  parseElement el =
    NuGetDependency <$> attr "id" el
                    <*> attr "version" el

buildGraph :: Nuspec -> Graphing Dependency
buildGraph project = unfold direct (const []) toDependency
    where
    direct = concatMap dependencies (groups project)
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
                 , dependencyName = depID
                 , dependencyVersion = Just (CEq depVersion)
                 , dependencyLocations = []
                 , dependencyTags = M.empty
                 }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
