module Strategy.Node.PackageJson
  ( discover
  , buildGraph

  , PackageJson(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.Error
import Polysemy.Output

import Diagnostics
import DepTypes
import Discovery.Walk
import Effect.LabeledGrapher
import Effect.ReadFS
import Graphing (Graphing)
import Types

discover :: Discover
discover = Discover
  { discoverName = "packagejson"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ subdirs files -> do
  case find (\f -> fileName f == "package.json") files of
    Nothing -> pure ()
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ (output . dummyConfigure "nodejs-packagejson" NotOptimal NotComplete (parent file)) res

  walkSkipNamed ["node_modules/"] subdirs

data PackageJson = PackageJson
  { packageDeps    :: Map Text Text
  , packageDevDeps :: Map Text Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PackageJson where
  parseJSON = withObject "PackageJson" $ \obj ->
    PackageJson <$> obj .:? "dependencies"    .!= M.empty
                <*> obj .:? "devDependencies" .!= M.empty

analyze :: Members '[ReadFS, Error ReadFSErr] r => Path Rel File -> Sem r (Graphing Dependency)
analyze file = do
  packageJson <- readContentsJson @PackageJson file
  pure (buildGraph packageJson)

-- TODO: decode version constraints
data NodePackage = NodePackage
  { pkgName       :: Text
  , pkgConstraint :: Text
  } deriving (Eq, Ord, Show, Generic)

type instance PkgLabel NodePackage = NodePackageLabel

newtype NodePackageLabel = NodePackageEnv Text
  deriving (Eq, Ord, Show, Generic)

buildGraph :: PackageJson -> Graphing Dependency
buildGraph PackageJson{..} = run . withLabeling toDependency $ do
  _ <- M.traverseWithKey (addDep "production") packageDeps
  _ <- M.traverseWithKey (addDep "development") packageDevDeps
  pure ()

  where

  addDep :: Member (LabeledGrapher NodePackage) r => Text -> Text -> Text -> Sem r ()
  addDep env name constraint = do
    let pkg = NodePackage name constraint
    direct pkg
    label pkg (NodePackageEnv env)

  toDependency :: NodePackage -> Set NodePackageLabel -> Dependency
  toDependency dep = foldr addLabel (start dep)

  addLabel :: NodePackageLabel -> Dependency -> Dependency
  addLabel (NodePackageEnv env) dep =
    dep { dependencyTags = M.insertWith (++) "environment" [env] (dependencyTags dep) }

  start :: NodePackage -> Dependency
  start NodePackage{..} = Dependency
    { dependencyType = NodeJSType
    , dependencyName = pkgName
    , dependencyVersion = Just (CCompatible pkgConstraint)
    , dependencyLocations = []
    , dependencyTags = M.empty
    }
