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
import Debug.Trace
import DepTypes
import Effect.ReadFS
import Graphing
import Path
import Strategy.Yarn.LockfileV2
import Strategy.Yarn.Resolvers

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m (Graphing Dependency)
analyze file = do
  lockfile <- readContentsYaml @YarnLockfile file
  pure (buildGraph lockfile)

buildGraph :: YarnLockfile -> Graphing Dependency
buildGraph = undefined

doSomething :: (Has ReadFS sig m, Has Diagnostics sig m) => Path b File -> m ()
doSomething file = do
  lockfile <- context "Reading lockfile" $ readContentsYaml @YarnLockfile file
  stitched <- context "Validating lockfile" $ stitchLockfile lockfile
  packageGraph <- context "Resolving yarn locators" $ fromEither (AME.gtraverse resolveLocatorToPackage stitched)
  traceM (show packageGraph)
  pure ()

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
    graph :: Has Diagnostics sig m => m (AM.AdjacencyMap Locator)
    graph = fmap (AM.edges . concat) . traverse packageToEdges . M.elems $ lockfile

------------------------

debug :: IO ()
--debug = bind (either (print . renderFailureBundle) (const (pure ()))) . runDiagnostics . runReadFSIO $ doSomething $(mkAbsFile "/Users/connor/Desktop/data-block-extract/yarn.lock")
debug = bind (either (print . renderFailureBundle) (const (pure ()))) . runDiagnostics . runReadFSIO $ doSomething $(mkAbsFile "/Users/connor/Desktop/tmp26/yarn.lock")

bind = (=<<)
