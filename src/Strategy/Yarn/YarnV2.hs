-- TODO: kill
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Yarn.YarnV2 (
  analyze,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Extra qualified as AME
import Control.Applicative ((<|>))
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Effect.Diagnostics
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import DepTypes
import Effect.ReadFS
import Graphing (Graphing)
import Graphing qualified
import Path
import Strategy.Yarn.LockfileV2
import Strategy.Yarn.Resolvers

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m (Graphing Dependency)
analyze file = context "Yarn" $ do
  lockfile <- context "Reading lockfile" $ readContentsYaml @YarnLockfile file
  stitched <- context "Validating lockfile" $ stitchLockfile lockfile
  packageGraph <- context "Resolving yarn locators" $ AME.gtraverse resolveLocatorToPackage stitched
  context "Building dependency graph" $ pure (buildGraph packageGraph)

-- | Validate and stitch together a yarn lockfile into a graph of yarn Locators
--
-- This ensures that all dependency relationships are valid
stitchLockfile :: Has Diagnostics sig m => YarnLockfile -> m (AM.AdjacencyMap Locator)
stitchLockfile (YarnLockfile lockfile) = graph
  where
    -- remapping @Map [Descriptor] PackageDescription@ to @Map Descriptor PackageDescription@
    remapped :: Map Descriptor PackageDescription
    remapped = M.fromList . concatMap (\(ks, v) -> map (,v) ks) . M.toList $ lockfile

    -- FIXME: doc about default npm: protocol
    lookupPackage :: Has Diagnostics sig m => Descriptor -> m PackageDescription
    lookupPackage desc =
      fromMaybeText ("Couldn't find package for descriptor: " <> T.pack (show desc)) $
        M.lookup desc remapped <|> M.lookup (desc{descriptorRange = "npm:" <> descriptorRange desc}) remapped

    -- look up all of a package's dependencies as locators in the lockfile
    lookupPackageDeps :: Has Diagnostics sig m => PackageDescription -> m [Locator]
    lookupPackageDeps = fmap (map descResolution) . traverse lookupPackage . descDependencies

    -- build the edges (adjacency list) between a package and its dependencies
    packageToEdges :: Has Diagnostics sig m => PackageDescription -> m [(Locator, Locator)]
    packageToEdges package = map (descResolution package,) <$> lookupPackageDeps package

    -- combine the edges produced by calling packageToEdges on each package in the lockfile
    graphEdges :: Has Diagnostics sig m => m (AM.AdjacencyMap Locator)
    graphEdges = fmap (AM.edges . concat) . traverse packageToEdges . M.elems $ lockfile

    -- not all packages will be part of an edge, so add vertices for each package
    graphVertices :: AM.AdjacencyMap Locator
    graphVertices = AM.vertices (map descResolution (M.elems lockfile))

    -- combine edges and vertices into a final graph
    graph :: Has Diagnostics sig m => m (AM.AdjacencyMap Locator)
    graph = AM.overlay graphVertices <$> graphEdges

-- | Turn a graph of packages into a dependency graph
--
-- Because workspaces are top-level projects, we set their dependencies as
-- direct in the dependency graph
buildGraph :: AM.AdjacencyMap Package -> Graphing Dependency
buildGraph gr = convertedGraphing
  where
    isWorkspace WorkspacePackage{} = True
    isWorkspace _ = False

    -- workspaces are the "direct" dependencies
    directPackages :: [Package]
    directPackages = filter isWorkspace (AM.vertexList gr)

    -- a Graphing containing only the direct deps
    directGraphing :: Graphing Package
    directGraphing = Graphing.fromList directPackages
    -- a Graphing containing the full Package graph, but without any deps marked as direct
    transitiveGraphing :: Graphing Package
    transitiveGraphing = Graphing.fromAdjacencyMap gr

    -- combine direct and transitive graphs; eliminate workspaces by stripping
    -- the root (the dependencies of the workspaces become direct dependencies)
    completeGraphing :: Graphing Package
    completeGraphing = Graphing.stripRoot $ directGraphing <> transitiveGraphing

    -- convert Packages in the graph to Dependencies
    convertedGraphing :: Graphing Dependency
    convertedGraphing = Graphing.induceJust . Graphing.gmap packageToDependency $ completeGraphing

-- | Convert a yarn package to a fossa Dependency
--
-- Dependency types that aren't supported return Nothing
packageToDependency :: Package -> Maybe Dependency
packageToDependency WorkspacePackage{} = Nothing
packageToDependency (NpmPackage maybeScope name version) =
  Just
    Dependency
      { dependencyType = NodeJSType
      , dependencyName =
          case maybeScope of
            Nothing -> name
            Just scope -> "@" <> scope <> "/" <> name
      , dependencyVersion = Just (CEq version)
      , dependencyLocations = []
      , dependencyTags = M.empty
      , dependencyEnvironments = []
      }
packageToDependency (GitPackage repo commit) =
  Just
    Dependency
      { dependencyType = GitType
      , dependencyName = repo
      , dependencyVersion = Just (CEq commit)
      , dependencyLocations = []
      , dependencyTags = M.empty
      , dependencyEnvironments = []
      }

------------------------

debug :: IO ()
debug = bind (either (print . renderFailureBundle) print) . runDiagnostics . runReadFSIO $ analyze $(mkAbsFile "/Users/connor/Desktop/data-block-extract/yarn.lock")

--debug = bind (either (print . renderFailureBundle) print) . runDiagnostics . runReadFSIO $ analyze $(mkAbsFile "/Users/connor/Desktop/tmp26/yarn.lock")

bind = (=<<)
