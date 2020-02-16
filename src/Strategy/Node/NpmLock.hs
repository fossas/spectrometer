module Strategy.Node.NpmLock
  ( discover
  , analyze
  , buildGraph

  , NpmPackageJson(..)
  , NpmDep(..)
  )
  where

import Prologue

import Control.Carrier.Error.Either
import Control.Carrier.Output.List
import qualified Data.Map.Strict as M
import Diagnostics
import DepTypes
import Discovery.Walk
import Effect.LabeledGrapher
import Effect.ReadFS
import Graphing (Graphing)
import Types

discover :: Discover
discover = Discover
  { discoverName = "npm-packagelock"
  , discoverFunc = discover'
  }

discover' ::
  ( Has ReadFS sig m
  , Has (Output ProjectClosure) sig m
  , MonadIO m
  , Effect sig
  )
  => Path Abs Dir -> m ()
discover' = walk $ \_ subdirs files -> do
  case find (\f -> fileName f == "package-lock.json") files of
    Nothing -> pure ()
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ output res

  walkSkipNamed ["node_modules/"] subdirs

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

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosure
analyze file = mkProjectClosure file <$> readContentsJson @NpmPackageJson file

mkProjectClosure :: Path Rel File -> NpmPackageJson -> ProjectClosure
mkProjectClosure file lock = ProjectClosure
  { closureStrategyGroup = NodejsGroup
  , closureStrategyName  = "nodejs-packagelock"
  , closureModuleDir     = parent file
  , closureDependencies  = dependencies
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph lock
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

data NpmPackage = NpmPackage
  { pkgName    :: Text
  , pkgVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

type instance PkgLabel NpmPackage = NpmPackageLabel

data NpmPackageLabel = NpmPackageEnv Text | NpmPackageLocation Text
  deriving (Eq, Ord, Show, Generic)

buildGraph :: NpmPackageJson -> Graphing Dependency
buildGraph packageJson = run . withLabeling toDependency $ do
  _ <- M.traverseWithKey addDirect (packageDependencies packageJson)
  _ <- M.traverseWithKey addDep (packageDependencies packageJson)
  pure ()

  where
  addDirect :: Has (LabeledGrapher NpmPackage) sig m => Text -> NpmDep -> m ()
  addDirect name NpmDep{depVersion} = direct (NpmPackage name depVersion)

  addDep :: Has (LabeledGrapher NpmPackage) sig m => Text -> NpmDep -> m ()
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

  toDependency :: NpmPackage -> Set NpmPackageLabel -> Dependency
  toDependency pkg = foldr addLabel (start pkg)

  addLabel :: NpmPackageLabel -> Dependency -> Dependency
  addLabel (NpmPackageEnv env) dep = dep { dependencyTags = M.insertWith (++) "environment" [env] (dependencyTags dep) }
  addLabel (NpmPackageLocation loc) dep = dep { dependencyLocations = loc : dependencyLocations dep }

  start :: NpmPackage -> Dependency
  start NpmPackage{..} = Dependency
    { dependencyType = NodeJSType
    , dependencyName = pkgName
    , dependencyVersion = Just $ CEq pkgVersion
    , dependencyLocations = []
    , dependencyTags = M.empty
    }
