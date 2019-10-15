module Strategy.Node.NpmLock
  ( discover
  , analyze
  , strategy
  )
  where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           Discovery.Walk
import           Effect.Graphing
import           Effect.ReadFS
import qualified Graph as G
import           Types

discover :: Discover
discover = Discover
  { discoverName = "npm-packagelock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ subdirs files -> do
  case find (\f -> fileName f == "package-lock.json") files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkSkipNamed ["node_modules/"] subdirs

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nodejs-packagelock"
  , strategyAnalyze = \opts -> analyze
      & fileInputJson @NpmPackageJson (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts

data NpmPackageJson = NpmPackageJson
  { packageName         :: Text
  , packageVersion      :: Text
  , packageDependencies :: Map Text NpmDep
  } deriving (Eq, Ord, Show, Generic)

data NpmDep = NpmDep
  { depVersion      :: Text
  , depDev          :: Maybe Bool
  , depResolved     :: Maybe Text
  , depRequires     :: Maybe (Map Text Text) -- ^ name to version spec
  , depDependencies :: Maybe (Map Text NpmDep)
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON NpmPackageJson where
  parseJSON = withObject "NpmPackageJson" $ \obj ->
    NpmPackageJson <$> obj .: "name"
                   <*> obj .: "version"
                   <*> obj .: "dependencies"

instance FromJSON NpmDep where
  parseJSON = withObject "NpmDep" $ \obj ->
    NpmDep <$> obj .:  "version"
           <*> obj .:? "dev"
           <*> obj .:? "resolved"
           <*> obj .:? "requires"
           <*> obj .:? "dependencies"

analyze :: Member (Input NpmPackageJson) r => Sem r G.Graph
analyze = buildGraph <$> input

data NpmPackage = NpmPackage
  { pkgName    :: Text
  , pkgVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

type instance PkgLabel NpmPackage = NpmPackageLabel

data NpmPackageLabel = NpmPackageEnv Text | NpmPackageLocation Text
  deriving (Eq, Ord, Show, Generic)

buildGraph :: NpmPackageJson -> G.Graph
buildGraph packageJson = run . graphingToGraph @NpmPackage toDependency $ do
  _ <- M.traverseWithKey addDirect (packageDependencies packageJson)
  _ <- M.traverseWithKey addDep (packageDependencies packageJson)
  pure ()

  where
  addDirect :: Member (Graphing NpmPackage) r => Text -> NpmDep -> Sem r ()
  addDirect name NpmDep{depVersion} = direct (NpmPackage name depVersion)

  addDep :: Member (Graphing NpmPackage) r => Text -> NpmDep -> Sem r ()
  addDep name NpmDep{..} = do
    let pkg = NpmPackage name depVersion

    case depDev of
      Just True -> label pkg (NpmPackageEnv "development")
      _         -> label pkg (NpmPackageEnv "production")

    traverse_ (label pkg . NpmPackageLocation) depResolved

    -- add edges to required packages
    case depRequires of
      Nothing -> pure ()
      Just required ->
        void $ M.traverseWithKey (\reqName reqVer -> edge pkg (NpmPackage reqName reqVer)) required

    -- add dependency nodes
    case depDependencies of
      Nothing -> pure ()
      Just deps ->
        void $ M.traverseWithKey addDep deps

  toDependency :: NpmPackage -> Set NpmPackageLabel -> G.Dependency
  toDependency pkg = foldr addLabel (start pkg)

  addLabel :: NpmPackageLabel -> G.Dependency -> G.Dependency
  addLabel (NpmPackageEnv env) dep = dep { G.dependencyTags = M.insertWith (++) "environment" [env] (G.dependencyTags dep) }
  addLabel (NpmPackageLocation loc) dep = dep { G.dependencyLocations = loc : G.dependencyLocations dep }

  start :: NpmPackage -> G.Dependency
  start NpmPackage{..} = G.Dependency
    { dependencyType = G.NodeJSType
    , dependencyName = pkgName
    , dependencyVersion = Just $ G.CEq pkgVersion
    , dependencyLocations = []
    , dependencyTags = M.empty
    }
