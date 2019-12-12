module Strategy.Node.PackageJson
  ( discover
  , strategy
  , buildGraph
  ) where

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
  { discoverName = "packagejson"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ subdirs files -> do
  case find (\f -> fileName f == "package.json") files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkSkipNamed ["node_modules/"] subdirs

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nodejs-packagejson"
  , strategyAnalyze = \opts -> analyze
      & fileInputJson @PackageJson (targetFile opts)
  , strategyModule = parent. targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

data PackageJson = PackageJson
  { packageDeps    :: Map Text Text
  , packageDevDeps :: Map Text Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PackageJson where
  parseJSON = withObject "PackageJson" $ \obj ->
    PackageJson <$> obj .:? "dependencies"    .!= M.empty
                <*> obj .:? "devDependencies" .!= M.empty

analyze :: Member (Input PackageJson) r => Sem r G.Graph
analyze = buildGraph <$> input

-- TODO: decode version constraints
data NodePackage = NodePackage
  { pkgName       :: Text
  , pkgConstraint :: Text
  } deriving (Eq, Ord, Show, Generic)

type instance PkgLabel NodePackage = NodePackageLabel

newtype NodePackageLabel = NodePackageEnv Text
  deriving (Eq, Ord, Show, Generic)

buildGraph :: PackageJson -> G.Graph
buildGraph PackageJson{..} = run . graphingToGraph toDependency $ do
  _ <- M.traverseWithKey (addDep "production") packageDeps
  _ <- M.traverseWithKey (addDep "development") packageDevDeps
  pure ()

  where

  addDep :: Member (Graphing NodePackage) r => Text -> Text -> Text -> Sem r ()
  addDep env name constraint = do
    let pkg = NodePackage name constraint
    direct pkg
    label pkg (NodePackageEnv env)

  toDependency :: NodePackage -> Set NodePackageLabel -> G.Dependency
  toDependency dep = foldr addLabel (start dep)

  addLabel :: NodePackageLabel -> G.Dependency -> G.Dependency
  addLabel (NodePackageEnv env) dep =
    dep { G.dependencyTags = M.insertWith (++) "environment" [env] (G.dependencyTags dep) }

  start :: NodePackage -> G.Dependency
  start NodePackage{..} = G.Dependency
    { dependencyType = G.NodeJSType
    , dependencyName = pkgName
    , dependencyVersion = Just (G.CCompatible pkgConstraint)
    , dependencyLocations = []
    , dependencyTags = M.empty
    }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
