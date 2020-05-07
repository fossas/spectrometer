module Strategy.Cargo
  ( discover
  , analyze
  )
  where

import Prologue

import Control.Effect.Error
import Data.Aeson.Types
import qualified Data.Map.Strict as M
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Types

import qualified Data.Text as T

data CargoLabel = 
    CargoDepKind DepEnvironment
  -- | CargoSource T.Text
  deriving (Eq, Ord, Show, Generic)

data PackageId = PackageId
  { pkgIdName :: T.Text
  , pkgIdVersion :: T.Text
  , pkgIdSource :: T.Text
  } deriving (Eq, Ord, Show, Generic)

data PackageDependency = PackageDependency
  { pkgDepName :: T.Text
  , pkgDepSource :: T.Text
  , pkgDepReq :: T.Text
  , pkgDepKind :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)

data Package = Package
  { pkgName :: T.Text
  , pkgVersion :: T.Text
  , pkgId :: PackageId
  , pkgLicense :: Maybe T.Text
  , pkgLicenseFile :: Maybe T.Text
  , pkgDependencies :: [PackageDependency]
  } deriving (Eq, Ord, Show, Generic)

data NodeDepKind = NodeDepKind
  { nodeDepKind :: Maybe T.Text
  , nodeDepTarget :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)

data NodeDependency = NodeDependency
  { nodePkg :: PackageId
  , nodeDepKinds :: [NodeDepKind]
  } deriving (Eq, Ord, Show, Generic)

data ResolveNode = ResolveNode
  { resolveNodeId :: PackageId
  , resolveNodeDeps :: [NodeDependency]
  } deriving (Eq, Ord, Show, Generic)

data Resolve = Resolve
  { resolvedNodes :: [ResolveNode] 
  } deriving (Eq, Ord, Show, Generic)

data CargoMetadata = CargoMetadata
  { metadataPackages :: [Package]
  , metadataWorkspaceMembers :: [PackageId]
  , metadataResolve :: Resolve
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PackageDependency where
  parseJSON = withObject "PackageDependency" $ \obj ->
    PackageDependency <$> obj .: "name"
                      <*> obj .: "source"
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

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files ->
  case find (\f -> fileName f == "Cargo.toml") files of
    Nothing -> pure WalkContinue
    Just file -> do
      runSimpleStrategy "rust-cargo" RustGroup $ fmap (mkProjectClosure dir) (analyze file)
      pure WalkSkipAll

cargoGenLockfileCmd :: Command
cargoGenLockfileCmd = Command
  { cmdNames = ["cargo"]
  , cmdBaseArgs = ["generate-lockfile"]
  , cmdAllowErr = Never
  }

cargoMetadataCmd :: Command
cargoMetadataCmd = Command
  { cmdNames = ["cargo"]
  , cmdBaseArgs = ["metadata"]
  , cmdAllowErr = Never
  }

mkProjectClosure :: Path Rel Dir ->  Graphing Dependency -> ProjectClosureBody
mkProjectClosure dir graph = ProjectClosureBody
  { bodyModuleDir    = dir
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = graph
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

analyze :: (Has Exec sig m, Has (Error ExecErr) sig m, Effect sig)
  => Path Rel File -> m (Graphing Dependency)
analyze manifest = do
  _ <- execThrow (parent manifest) cargoGenLockfileCmd []
  meta <- execJson @CargoMetadata (parent manifest) cargoMetadataCmd []
  --
  withLabeling toDependency $ buildGraph meta

toDependency :: PackageId -> Set CargoLabel -> Dependency
toDependency pkg = foldr applyLabel Dependency 
  { dependencyType = CargoType 
  , dependencyName = pkgIdName pkg
  , dependencyVersion = Just $ CEq $ pkgIdVersion pkg
  , dependencyLocations = []
  , dependencyEnvironments = []
  , dependencyTags = M.empty
  }
  where
    applyLabel :: CargoLabel -> Dependency -> Dependency
    applyLabel (CargoDepKind env) dep = dep { dependencyEnvironments = env : dependencyEnvironments dep }

-- Possible values here are "build", "dev", and null.
-- Null refers to productions, while dev and build refer to development-time dependencies
-- Cargo does not differentiate test dependencies and dev dependencies,
-- so we just simplify it to Development.
kindToLabel :: Maybe T.Text -> CargoLabel
kindToLabel (Just _) = CargoDepKind EnvDevelopment
kindToLabel Nothing  = CargoDepKind EnvProduction

addLabel_ :: Has (LabeledGrapher PackageId CargoLabel) sig m => NodeDependency -> m ()
addLabel_ dep = do
  let pkgId = nodePkg dep
  traverse_ (label pkgId . kindToLabel . nodeDepKind) $ nodeDepKinds dep

addEdge :: Has (LabeledGrapher PackageId CargoLabel) sig m => ResolveNode -> m ()
addEdge node = do
  let parentId = resolveNodeId node
  for_ (resolveNodeDeps node) $ \dep -> do
    addLabel_ dep
    edge parentId $ nodePkg dep

buildGraph :: Has (LabeledGrapher PackageId CargoLabel) sig m => CargoMetadata -> m ()
buildGraph meta = do
  traverse_ direct $ metadataWorkspaceMembers meta
  traverse_ addEdge $ resolvedNodes $ metadataResolve meta

parsePkgId :: T.Text -> Parser PackageId
parsePkgId t = 
  case T.splitOn " " t of
    [a,b,c] -> pure $ PackageId a b c
    _ -> fail "malformed Package ID"
