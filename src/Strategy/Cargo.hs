module Strategy.Cargo (
  discover,
  CargoMetadata (..),
  NodeDependency (..),
  NodeDepKind (..),
  PackageId (..),
  Resolve (..),
  ResolveNode (..),
  buildGraph,
  getDeps,
  mkProject,
  findProjects,
) where

import Control.Effect.Diagnostics
import Data.Aeson.Types
import Data.Foldable (for_, traverse_)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Text qualified as T
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Effect.ReadFS (ReadFS)
import Graphing (Graphing, stripRoot)
import Path
import Types

newtype CargoLabel
  = CargoDepKind DepEnvironment
  deriving (Eq, Ord, Show)

data PackageId = PackageId
  { pkgIdName :: T.Text
  , pkgIdVersion :: T.Text
  , pkgIdSource :: T.Text
  }
  deriving (Eq, Ord, Show)

data PackageDependency = PackageDependency
  { pkgDepName :: T.Text
  , pkgDepReq :: T.Text
  , pkgDepKind :: Maybe T.Text
  }
  deriving (Eq, Ord, Show)

data Package = Package
  { pkgName :: T.Text
  , pkgVersion :: T.Text
  , pkgId :: PackageId
  , pkgLicense :: Maybe T.Text
  , pkgLicenseFile :: Maybe T.Text
  , pkgDependencies :: [PackageDependency]
  }
  deriving (Eq, Ord, Show)

data NodeDepKind = NodeDepKind
  { nodeDepKind :: Maybe T.Text
  , nodeDepTarget :: Maybe T.Text
  }
  deriving (Eq, Ord, Show)

data NodeDependency = NodeDependency
  { nodePkg :: PackageId
  , nodeDepKinds :: [NodeDepKind]
  }
  deriving (Eq, Ord, Show)

data ResolveNode = ResolveNode
  { resolveNodeId :: PackageId
  , resolveNodeDeps :: [NodeDependency]
  }
  deriving (Eq, Ord, Show)

newtype Resolve = Resolve
  { resolvedNodes :: [ResolveNode]
  }
  deriving (Eq, Ord, Show)

data CargoMetadata = CargoMetadata
  { metadataPackages :: [Package]
  , metadataWorkspaceMembers :: [PackageId]
  , metadataResolve :: Resolve
  }
  deriving (Eq, Ord, Show)

instance FromJSON PackageDependency where
  parseJSON = withObject "PackageDependency" $ \obj ->
    PackageDependency <$> obj .: "name"
      <*> obj .: "req"
      <*> obj .:? "kind"

instance FromJSON Package where
  parseJSON = withObject "Package" $ \obj ->
    Package <$> obj .: "name"
      <*> obj .: "version"
      <*> (obj .: "id" >>= parsePkgId)
      <*> obj .:? "license"
      <*> obj .:? "license_file"
      <*> obj .: "dependencies"

instance FromJSON NodeDepKind where
  parseJSON = withObject "NodeDepKind" $ \obj ->
    NodeDepKind <$> obj .:? "kind"
      <*> obj .:? "target"

instance FromJSON NodeDependency where
  parseJSON = withObject "NodeDependency" $ \obj ->
    NodeDependency <$> (obj .: "pkg" >>= parsePkgId)
      <*> obj .: "dep_kinds"

instance FromJSON ResolveNode where
  parseJSON = withObject "ResolveNode" $ \obj ->
    ResolveNode <$> (obj .: "id" >>= parsePkgId)
      <*> obj .: "deps"

instance FromJSON Resolve where
  parseJSON = withObject "Resolve" $ \obj ->
    Resolve <$> obj .: "nodes"

instance FromJSON CargoMetadata where
  parseJSON = withObject "CargoMetadata" $ \obj ->
    CargoMetadata <$> obj .: "packages"
      <*> (obj .: "workspace_members" >>= traverse parsePkgId)
      <*> obj .: "resolve"

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Cargo" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [CargoProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "Cargo.toml" files of
    Nothing -> pure ([], WalkContinue)
    Just toml -> do
      let project =
            CargoProject
              { cargoToml = toml
              , cargoDir = dir
              }

      pure ([project], WalkSkipAll)

data CargoProject = CargoProject
  { cargoDir :: Path Abs Dir
  , cargoToml :: Path Abs File
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has Exec sig n, Has Diagnostics sig n) => CargoProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "cargo"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ getDeps project
    , projectPath = cargoDir project
    , projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has Diagnostics sig m) => CargoProject -> m (Graphing Dependency, GraphBreadth)
getDeps = context "Cargo" . context "Dynamic analysis" . analyze . cargoDir

cargoGenLockfileCmd :: Command
cargoGenLockfileCmd =
  Command
    { cmdName = "cargo"
    , cmdArgs = ["generate-lockfile"]
    , cmdAllowErr = Never
    }

cargoMetadataCmd :: Command
cargoMetadataCmd =
  Command
    { cmdName = "cargo"
    , cmdArgs = ["metadata"]
    , cmdAllowErr = Never
    }

analyze ::
  (Has Exec sig m, Has Diagnostics sig m) =>
  Path Abs Dir ->
  m (Graphing Dependency, GraphBreadth)
analyze manifestDir = do
  _ <- context "Generating lockfile" $ execThrow manifestDir cargoGenLockfileCmd
  meta <- execJson @CargoMetadata manifestDir cargoMetadataCmd
  --
  graph <- context "Building dependency graph" $ pure (buildGraph meta)
  pure (graph, Complete)

toDependency :: PackageId -> Set CargoLabel -> Dependency
toDependency pkg =
  foldr
    applyLabel
    Dependency
      { dependencyType = CargoType
      , dependencyName = pkgIdName pkg
      , dependencyVersion = Just $ CEq $ pkgIdVersion pkg
      , dependencyLocations = []
      , dependencyEnvironments = []
      , dependencyTags = M.empty
      }
  where
    applyLabel :: CargoLabel -> Dependency -> Dependency
    applyLabel (CargoDepKind env) dep = dep{dependencyEnvironments = env : dependencyEnvironments dep}

-- Possible values here are "build", "dev", and null.
-- Null refers to productions, while dev and build refer to development-time dependencies
-- Cargo does not differentiate test dependencies and dev dependencies,
-- so we just simplify it to Development.
kindToLabel :: Maybe T.Text -> CargoLabel
kindToLabel (Just _) = CargoDepKind EnvDevelopment
kindToLabel Nothing = CargoDepKind EnvProduction

addLabel :: Has (LabeledGrapher PackageId CargoLabel) sig m => NodeDependency -> m ()
addLabel dep = do
  let packageId = nodePkg dep
  traverse_ (label packageId . kindToLabel . nodeDepKind) $ nodeDepKinds dep

addEdge :: Has (LabeledGrapher PackageId CargoLabel) sig m => ResolveNode -> m ()
addEdge node = do
  let parentId = resolveNodeId node
  for_ (resolveNodeDeps node) $ \dep -> do
    addLabel dep
    edge parentId $ nodePkg dep

buildGraph :: CargoMetadata -> Graphing Dependency
buildGraph meta = stripRoot $
  run . withLabeling toDependency $ do
    traverse_ direct $ metadataWorkspaceMembers meta
    traverse_ addEdge $ resolvedNodes $ metadataResolve meta

parsePkgId :: T.Text -> Parser PackageId
parsePkgId t =
  case T.splitOn " " t of
    [a, b, c] -> pure $ PackageId a b c
    _ -> fail "malformed Package ID"
