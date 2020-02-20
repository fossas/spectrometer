module Strategy.NuGet.PackagesConfig
  ( discover
  , buildGraph
  , analyze

  , PackagesConfig(..)
  , NuGetDependency(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Parse.XML
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> (fileName f) == "packages.config") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "nuget-packagesconfig" DotnetGroup $ analyze file

  walkContinue

instance FromXML PackagesConfig where
  parseElement el = PackagesConfig <$> children "package" el

instance FromXML NuGetDependency where
  parseElement el =
    NuGetDependency <$> attr "id" el
                    <*> attr "version" el

newtype PackagesConfig = PackagesConfig
  { deps :: [NuGetDependency]
  } deriving (Eq, Ord, Show, Generic)

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosure
analyze file = mkProjectClosure file <$> readContentsXML file

mkProjectClosure :: Path Rel File -> PackagesConfig -> ProjectClosure
mkProjectClosure file config = ProjectClosure
  { closureStrategyGroup = DotnetGroup
  , closureStrategyName  = "nuget-packagesconfig"
  , closureModuleDir     = parent file
  , closureDependencies  = dependencies
  , closureLicenses      = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph config
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

data NuGetDependency = NuGetDependency
  { depID      :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

buildGraph :: PackagesConfig -> Graphing Dependency
buildGraph config = unfold (deps config) (const []) toDependency
    where
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = depID
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyTags = M.empty
               }
