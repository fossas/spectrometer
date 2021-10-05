{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Node (
  discover,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Extra qualified as AME
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  errorBoundary,
  fatalText,
  fromEither,
  fromEitherShow,
  fromMaybeText,
  renderFailureBundle,
 )
import Control.Monad ((<=<))
import Data.Glob (Glob)
import Data.Glob qualified as Glob
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (decodeUtf8)
import Data.Tagged (applyTag)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Walk (
  WalkStep (WalkSkipSome),
  findFileNamed,
  walk',
 )
import Effect.Logger (
  Logger,
  logError,
 )
import Effect.ReadFS (
  ReadFS,
  doesFileExist,
  readContentsBSLimit,
  readContentsJson,
 )
import Path (Abs, Dir, Path, Rel, mkRelFile, parent, (</>))
import Strategy.Node.Npm.PackageLock qualified as PackageLock
import Strategy.Node.PackageJson (
  Development,
  FileGraph,
  FlatDeps (FlatDeps),
  Manifest,
  NodePackage (NodePackage),
  PackageJson (..),
  PkgJsonGraph (..),
  PkgJsonWorkspaces (unWorkspaces),
  Production,
 )
import Strategy.Node.PackageJson qualified as PackageJson
import Strategy.Node.YarnV1.YarnLock qualified as V1
import Strategy.Node.YarnV2.YarnLock qualified as V2
import Types (
  DependencyResults (DependencyResults),
  DiscoveredProject (..),
  FoundTargets (ProjectWithoutTargets),
  GraphBreadth (Complete, Partial),
 )

skipJsFolders :: WalkStep
skipJsFolders = WalkSkipSome ["node_modules", "bower_components", ".yarn"]

discover ::
  ( Has Diagnostics rsig run
  , Has Logger rsig run
  , Has ReadFS rsig run
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has ReadFS sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject run]
discover dir = context "NodeJS" $ do
  manifestList <- context "Finding nodejs projects" $ collectManifests dir
  manifestMap <- context "Reading package.json files" $ Map.fromList <$> traverse loadPackage manifestList
  globalGraph <- context "Building global workspace graph" $ pure $ buildManifestGraph manifestMap
  graphs <- context "Splitting global graph into chunks" $ fromMaybeText "" $ splitGraph globalGraph
  context "Converting graphs to analysis targets" $ traverse (mkProject <=< identifyProjectType) graphs

collectManifests :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [Manifest]
collectManifests = walk' $ \_ _ files ->
  case findFileNamed "package.json" files of
    Nothing -> pure ([], skipJsFolders)
    Just jsonFile -> pure ([jsonFile], skipJsFolders)

mkProject ::
  ( Has ReadFS rsig run
  , Has Diagnostics rsig run
  , Has Logger rsig run
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  NodeProject ->
  m (DiscoveredProject run)
mkProject project = do
  let graph = case project of
        Yarn _ g -> g
        NPMLock _ g -> g
        NPM g -> g
  result <- errorBoundary $ fromEitherShow $ findWorkspaceRootManifest graph
  rootManifest <- case result of
    Left bundle -> do
      logError $ renderFailureBundle bundle
      fatalText "aborting NodeJS project creation"
    Right manifest -> do
      pure manifest
  pure $
    DiscoveredProject
      { projectType = "nodejs"
      , projectPath = parent rootManifest
      , projectBuildTargets = ProjectWithoutTargets
      , projectDependencyResults = const $ getDeps project
      , projectLicenses = pure []
      }

-- Since we don't natively support workspaces, we don't attempt to preserve them from this point on.
-- In the future, if you're adding generalized workspace support, start here.
getDeps ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  NodeProject ->
  m DependencyResults
getDeps (Yarn yarnLockFile graph) = analyzeYarn yarnLockFile graph
getDeps (NPMLock packageLockFile _) = analyzeNpmLock packageLockFile
getDeps (NPM graph) = analyzeNpm graph

analyzeNpmLock :: (Has Diagnostics sig m, Has ReadFS sig m) => Manifest -> m DependencyResults
analyzeNpmLock file = do
  graph <- PackageLock.analyze file
  pure $ DependencyResults graph Complete [file]

analyzeNpm :: (Has Diagnostics sig m) => PkgJsonGraph -> m DependencyResults
analyzeNpm wsGraph = do
  graph <- PackageJson.analyze $ Map.elems $ jsonLookup wsGraph
  pure $ DependencyResults graph Partial $ AM.vertexList $ jsonGraph wsGraph

analyzeYarn ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  , Has Logger sig m
  ) =>
  Manifest ->
  PkgJsonGraph ->
  m DependencyResults
analyzeYarn yarnLockFile pkgJsonGraph = do
  yarnVersion <- detectYarnVersion yarnLockFile
  let analyzeFunc = case yarnVersion of
        V1 -> V1.analyze
        V2Compatible -> V2.analyze

  graph <- analyzeFunc yarnLockFile $ extractDepLists pkgJsonGraph
  pure . DependencyResults graph Complete $ yarnLockFile : AM.vertexList (jsonGraph pkgJsonGraph)

detectYarnVersion ::
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Manifest ->
  m YarnVersion
detectYarnVersion yarnfile = do
  -- we expect the v1 header to end at char 82
  contents <- decodeUtf8 <$> (readContentsBSLimit yarnfile 100 >>= fromEither)
  if "yarn lockfile v1" `Text.isInfixOf` contents
    then pure V1
    else pure V2Compatible

data YarnVersion
  = V1
  | V2Compatible

extractDepLists :: PkgJsonGraph -> FlatDeps
extractDepLists PkgJsonGraph{..} = foldMap extractSingle $ Map.elems jsonLookup
  where
    mapToSet :: Map Text Text -> Set NodePackage
    mapToSet = Set.fromList . map (uncurry NodePackage) . Map.toList

    extractSingle :: PackageJson -> FlatDeps
    extractSingle PackageJson{..} =
      FlatDeps
        (applyTag @Production $ mapToSet packageDeps)
        (applyTag @Development $ mapToSet packageDevDeps)
        (Map.keysSet jsonLookup)

loadPackage :: (Has ReadFS sig m, Has Diagnostics sig m) => Manifest -> m (Manifest, PackageJson)
loadPackage file = do
  contents <- readContentsJson file
  pure (file, contents)

buildManifestGraph :: Map Manifest PackageJson -> PkgJsonGraph
buildManifestGraph manifestMap = PkgJsonGraph adjmap manifestMap
  where
    -- Run 'go' on each key/value pair: (file path, parsed contents of that file)
    adjmap :: FileGraph
    adjmap =
      manifestVertices
        `AM.overlay` Map.foldrWithKey
          (\k v m -> AM.overlay m $ go k v)
          AM.empty
          manifestMap

    -- Make sure all manifests end up in the graph, ignoring workspaces
    manifestVertices :: FileGraph
    manifestVertices = AM.vertices $ Map.keys manifestMap

    -- For a single package.json, find all direct children (in terms of workspaces).
    go :: Manifest -> PackageJson -> FileGraph
    go path pkgJson =
      foldr (\g m -> AM.overlay m $ findWorkspaceChildren path g) AM.empty
        . unWorkspaces
        $ packageWorkspaces pkgJson

    -- Given a workspace pattern, find all matches in the list of known manifest files.
    -- When found, create edges between the root path and the matching children.
    findWorkspaceChildren :: Manifest -> Glob Rel -> FileGraph
    findWorkspaceChildren path glob =
      manifestEdges path . filter (filterfunc path glob) $
        Map.keys manifestMap

    -- True if qualified glob pattern matches the given file.
    filterfunc :: Manifest -> Glob Rel -> Manifest -> Bool
    filterfunc root glob candidate = candidate `Glob.matches` qualifyGlobPattern root glob

    -- Yarn appends the filename to the glob, so we match that behavior
    -- https://github.com/yarnpkg/yarn/blob/master/src/config.js#L821
    qualifyGlobPattern :: Manifest -> Glob Rel -> Glob Abs
    qualifyGlobPattern root = Glob.append "package.json" . Glob.prefixWith (parent root)

    -- Create edges from a parent to its children
    manifestEdges :: Manifest -> [Manifest] -> FileGraph
    manifestEdges path children = AM.edges $ map (path,) children

splitGraph :: PkgJsonGraph -> Maybe [PkgJsonGraph]
splitGraph PkgJsonGraph{..} = map splitFromParent <$> AME.splitGraph jsonGraph
  where
    splitFromParent :: FileGraph -> PkgJsonGraph
    splitFromParent graph = PkgJsonGraph graph $ extractMapChunk graph jsonLookup

    extractMapChunk :: Ord k => AM.AdjacencyMap k -> Map k a -> Map k a
    extractMapChunk graph bigmap = Map.fromList . mapMaybe (getpair bigmap) $ AM.vertexList graph

    getpair :: Ord k => Map k a -> k -> Maybe (k, a)
    getpair mapping k = do
      val <- mapping Map.!? k
      pure (k, val)

identifyProjectType ::
  ( Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  PkgJsonGraph ->
  m NodeProject
identifyProjectType graph = do
  manifest <- fromEitherShow $ findWorkspaceRootManifest graph
  let yarnFilePath = parent manifest Path.</> $(mkRelFile "yarn.lock")
      packageLockPath = parent manifest Path.</> $(mkRelFile "package-lock.json")
  yarnExists <- doesFileExist yarnFilePath
  pkgLockExists <- doesFileExist packageLockPath
  pure $ case (yarnExists, pkgLockExists) of
    (True, _) -> Yarn yarnFilePath graph
    (_, True) -> NPMLock packageLockPath graph
    _ -> NPM graph

data NodeProject
  = Yarn Manifest PkgJsonGraph
  | NPMLock Manifest PkgJsonGraph
  | NPM PkgJsonGraph

findWorkspaceRootManifest :: PkgJsonGraph -> Either String Manifest
findWorkspaceRootManifest PkgJsonGraph{jsonGraph} =
  case AM.vertexList $ AM.induce (hasNoIncomingEdges jsonGraph) jsonGraph of
    [x] -> Right x
    _ -> Left "package.json workspace graph must have exactly 1 root manifest"

hasNoIncomingEdges :: FileGraph -> Manifest -> Bool
hasNoIncomingEdges graph item = Set.null $ AM.preSet item graph
