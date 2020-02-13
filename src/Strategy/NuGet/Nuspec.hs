module Strategy.NuGet.Nuspec
  ( discover
  , buildGraph
  , analyze

  , Nuspec(..)
  , Group(..)
  , NuGetDependency(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Polysemy
import Polysemy.Error
import Polysemy.Output

import Diagnostics
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Parse.XML
import Types

discover :: Discover
discover = Discover
  { discoverName = "nuspec"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Output ProjectClosure] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> L.isSuffixOf ".nuspec" (fileName f)) files of
    Nothing -> pure ()
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ output res

  walkContinue

analyze :: Members '[ReadFS, Error ReadFSErr] r => Path Rel File -> Sem r ProjectClosure
analyze file = do
  nuspec <- readContentsXML @Nuspec file
  pure (mkProjectClosure file nuspec)

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
