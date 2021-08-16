{-# LANGUAGE RecordWildCards #-}

module Srclib.Converter (
  toSourceUnit,
  depTypeToFetcher,
  fetcherToDepType,
) where

import Prelude

import Algebra.Graph.AdjacencyMap qualified as AM
import App.Fossa.Analyze.Project
import Control.Applicative ((<|>))
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes
import Graphing (Graphing)
import Graphing qualified
import Path (toFilePath)
import Srclib.Types

toSourceUnit :: ProjectResult -> SourceUnit
toSourceUnit ProjectResult{..} =
  SourceUnit
    { sourceUnitName = renderedPath
    , sourceUnitType = projectResultType
    , sourceUnitManifest = renderedPath
    , sourceUnitBuild =
        Just $
          SourceUnitBuild
            { buildArtifact = "default"
            , buildSucceeded = True
            , buildImports = imports
            , buildDependencies = deps
            }
    , sourceUnitGraphBreadth = projectResultGraphBreadth
    , additionalData = Nothing
    }
  where
    renderedPath = toText (toFilePath projectResultPath)

    filteredGraph :: Graphing Dependency
    filteredGraph = Graphing.shrink (\d -> shouldPublishDep d && isSupportedType d) projectResultGraph

    locatorGraph :: Graphing Locator
    locatorGraph = Graphing.gmap toLocator filteredGraph

    locatorAdjacent :: AM.AdjacencyMap Locator
    locatorAdjacent = Graphing.toAdjacencyMap locatorGraph

    deps :: [SourceUnitDependency]
    deps = map (mkSourceUnitDependency locatorAdjacent) (AM.vertexList locatorAdjacent)

    imports :: [Locator]
    imports = Graphing.directList locatorGraph

mkSourceUnitDependency :: AM.AdjacencyMap Locator -> Locator -> SourceUnitDependency
mkSourceUnitDependency gr locator =
  SourceUnitDependency
    { sourceDepLocator = locator
    , sourceDepImports = Set.toList $ AM.postSet locator gr
    }

shouldPublishDep :: Dependency -> Bool
shouldPublishDep Dependency{dependencyEnvironments} =
  null dependencyEnvironments || EnvProduction `elem` dependencyEnvironments || any isOtherEnv dependencyEnvironments

isOtherEnv :: DepEnvironment -> Bool
isOtherEnv (EnvOther _) = True
isOtherEnv _ = False

-- core can't handle subprojects
isSupportedType :: Dependency -> Bool
isSupportedType Dependency{dependencyType} = dependencyType /= SubprojectType && dependencyType /= GooglesourceType

toLocator :: Dependency -> Locator
toLocator dep =
  Locator
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
  ArchiveType -> "archive"
  CarthageType -> "cart"
  CargoType -> "cargo"
  ComposerType -> "comp"
  CondaType -> "conda"
  CpanType -> "cpan"
  CustomType -> "custom"
  GemType -> "gem"
  GitType -> "git"
  GooglesourceType -> "git" -- FIXME. Yet another thing that's coming back to bite us
  GoType -> "go"
  HackageType -> "hackage"
  HexType -> "hex"
  MavenType -> "mvn"
  NodeJSType -> "npm"
  NuGetType -> "nuget"
  PipType -> "pip"
  PodType -> "pod"
  RPMType -> "rpm"
  SubprojectType -> "mvn" -- FIXME. I knew SubprojectType would come back to bite us.
  URLType -> "url"
  UserType -> "user"

-- | GooglesourceType and SubprojectType are not supported with this function, since they're ambiguous.
fetcherToDepType :: Text -> Maybe DepType
fetcherToDepType fetcher | depTypeToFetcher ArchiveType == fetcher = Just ArchiveType
fetcherToDepType fetcher | depTypeToFetcher CarthageType == fetcher = Just CarthageType
fetcherToDepType fetcher | depTypeToFetcher CargoType == fetcher = Just CargoType
fetcherToDepType fetcher | depTypeToFetcher ComposerType == fetcher = Just ComposerType
fetcherToDepType fetcher | depTypeToFetcher CondaType == fetcher = Just CondaType
fetcherToDepType fetcher | depTypeToFetcher CpanType == fetcher = Just CpanType
fetcherToDepType fetcher | depTypeToFetcher CustomType == fetcher = Just CustomType
fetcherToDepType fetcher | depTypeToFetcher GemType == fetcher = Just GemType
fetcherToDepType fetcher | depTypeToFetcher GitType == fetcher = Just GitType
fetcherToDepType fetcher | depTypeToFetcher GoType == fetcher = Just GoType
fetcherToDepType fetcher | depTypeToFetcher HackageType == fetcher = Just HackageType
fetcherToDepType fetcher | depTypeToFetcher HexType == fetcher = Just HexType
fetcherToDepType fetcher | depTypeToFetcher MavenType == fetcher = Just MavenType
fetcherToDepType fetcher | depTypeToFetcher NodeJSType == fetcher = Just NodeJSType
fetcherToDepType fetcher | depTypeToFetcher NuGetType == fetcher = Just NuGetType
fetcherToDepType fetcher | depTypeToFetcher PipType == fetcher = Just PipType
fetcherToDepType fetcher | depTypeToFetcher PodType == fetcher = Just PodType
fetcherToDepType fetcher | depTypeToFetcher RPMType == fetcher = Just RPMType
fetcherToDepType fetcher | depTypeToFetcher URLType == fetcher = Just URLType
fetcherToDepType fetcher | depTypeToFetcher UserType == fetcher = Just UserType
fetcherToDepType _ = Nothing
