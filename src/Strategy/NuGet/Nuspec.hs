module Strategy.NuGet.Nuspec
  ( discover
  , buildGraph
  , analyze

  , Nuspec(..)
  , Group(..)
  , NuGetDependency(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M
import qualified Data.List as L

import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Parse.XML
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> L.isSuffixOf ".nuspec" (fileName f)) files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "nuget-nuspec" DotnetGroup $ analyze file

  walkContinue

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosure
analyze file = mkProjectClosure file <$> readContentsXML @Nuspec file

mkProjectClosure :: Path Rel File -> Nuspec -> ProjectClosure
mkProjectClosure file nuspec = ProjectClosure
  { closureStrategyGroup = DotnetGroup
  , closureStrategyName  = "nuget-nuspec"
  , closureModuleDir     = parent file
  , closureDependencies  = dependencies
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph nuspec
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

newtype Nuspec = Nuspec
  { groups :: [Group]
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
