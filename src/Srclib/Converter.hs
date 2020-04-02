module Srclib.Converter
  ( toSourceUnit
  ) where

import Prelude

import qualified Algebra.Graph.AdjacencyMap as AM
import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import DepTypes
import Graphing (Graphing)
import qualified Graphing
import Srclib.Types
import Types

toSourceUnit :: ProjectClosure -> SourceUnit
toSourceUnit ProjectClosure{..} = SourceUnit
  { sourceUnitName = renderedPath
  , sourceUnitType = SourceUnitTypeDummyCLI
  , sourceUnitManifest = renderedPath
  , sourceUnitBuild = SourceUnitBuild
    { buildArtifact = "default"
    , buildSucceeded = True
    , buildImports = imports
    , buildDependencies = deps
    }
  }
  where
    imports :: [Locator]
    imports = Set.toList $ Graphing.graphingDirect locatorGraph

    deps :: [SourceUnitDependency]
    deps = map (mkSourceUnitDependency locatorAdjacent) (AM.vertexList locatorAdjacent)

    locatorAdjacent :: AM.AdjacencyMap Locator
    locatorAdjacent = Graphing.graphingAdjacent locatorGraph

    locatorGraph :: Graphing Locator
    locatorGraph = Graphing.gmap toLocator filteredGraph

    filteredGraph :: Graphing Dependency
    filteredGraph = Graphing.filter (\d -> isProdDep d && isSupportedType d) graph

    graph :: Graphing Dependency
    graph = dependenciesGraph $ closureDependencies

    renderedPath :: Text
    renderedPath = Text.pack (show closureModuleDir) <> "/" <> closureStrategyName

mkSourceUnitDependency :: AM.AdjacencyMap Locator -> Locator -> SourceUnitDependency
mkSourceUnitDependency gr locator = SourceUnitDependency
  { sourceDepLocator = locator
  , sourceDepImports = Set.toList $ AM.postSet locator gr
  }

isProdDep :: Dependency -> Bool
isProdDep Dependency{dependencyEnvironments} =
  null dependencyEnvironments || EnvProduction `elem` dependencyEnvironments

-- core can't handle subprojects
isSupportedType :: Dependency -> Bool
isSupportedType Dependency{dependencyType} = dependencyType /= SubprojectType

toLocator :: Dependency -> Locator
toLocator dep = Locator
  { locatorFetcher = depTypeToFetcher (dependencyType dep)
  , locatorProject = dependencyName dep
  , locatorRevision = verConstraintToRevision =<< dependencyVersion dep
  }

verConstraintToRevision :: VerConstraint -> Maybe Text
verConstraintToRevision = \case
  CEq ver -> Just ver
  CURI _ -> Nothing -- we can't represent this in a locator
  CCompatible ver -> Just ver
  CAnd a b -> verConstraintToRevision a <|> verConstraintToRevision b
  COr a b -> verConstraintToRevision a <|> verConstraintToRevision b
  CLess ver -> Just ver -- ugh
  CLessOrEq ver -> Just ver -- ugh
  CGreater ver -> Just ver -- ugh
  CGreaterOrEq ver -> Just ver -- ugh
  CNot _ -> Nothing -- we can't represent this in a locator

depTypeToFetcher :: DepType -> Text
depTypeToFetcher = \case
  SubprojectType -> "mvn" -- FIXME. I knew SubprojectType would come back to bite us.
  GemType -> "gem"
  MavenType -> "mvn"
  NodeJSType -> "npm"
  NuGetType -> "nuget"
  PipType -> "pip"
  PodType -> "pod"
  GoType -> "go"
  CarthageType -> "cart"
