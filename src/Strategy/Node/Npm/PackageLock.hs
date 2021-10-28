{-# LANGUAGE RecordWildCards #-}

module Strategy.Node.Npm.PackageLock (
  analyze,
  buildGraph,
  NpmPackageJson (..),
  NpmDep (..),
) where

import Control.Effect.Diagnostics
import Data.Aeson
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import DepTypes
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Path

data NpmPackageJson = NpmPackageJson
  { packageName :: Text
  , packageVersion :: Text
  , packageDependencies :: Map Text NpmDep
  }
  deriving (Eq, Ord, Show)

data NpmDep = NpmDep
  { depVersion :: Text
  , depDev :: Maybe Bool
  , depResolved :: Maybe Text
  , -- | name to version spec
    depRequires :: Maybe (Map Text Text)
  , depDependencies :: Maybe (Map Text NpmDep)
  }
  deriving (Eq, Ord, Show)

instance FromJSON NpmPackageJson where
  parseJSON = withObject "NpmPackageJson" $ \obj ->
    NpmPackageJson <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .: "dependencies"

instance FromJSON NpmDep where
  parseJSON = withObject "NpmDep" $ \obj ->
    NpmDep <$> obj .: "version"
      <*> obj .:? "dev"
      <*> obj .:? "resolved"
      <*> obj .:? "requires"
      <*> obj .:? "dependencies"

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze file = do
  packageJson <- readContentsJson @NpmPackageJson file
  context "Building dependency graph" $ pure (buildGraph packageJson)

data NpmPackage = NpmPackage
  { pkgName :: Text
  , pkgVersion :: Text
  }
  deriving (Eq, Ord, Show)

type NpmGrapher = LabeledGrapher NpmPackage NpmPackageLabel

data NpmPackageLabel = NpmPackageEnv DepEnvironment | NpmPackageLocation Text
  deriving (Eq, Ord, Show)

buildGraph :: NpmPackageJson -> Graphing Dependency
buildGraph packageJson = run . withLabeling toDependency $ do
  _ <- Map.traverseWithKey addDirect (packageDependencies packageJson)
  _ <- Map.traverseWithKey addDep (packageDependencies packageJson)
  pure ()
  where
    addDirect :: Has NpmGrapher sig m => Text -> NpmDep -> m ()
    addDirect name NpmDep{depVersion} = direct (NpmPackage name depVersion)

    addDep :: Has NpmGrapher sig m => Text -> NpmDep -> m ()
    addDep name NpmDep{..} = do
      let pkg = NpmPackage name depVersion

      -- Allow entry of orphan deps.  We may prune these later.
      deep pkg

      case depDev of
        Just True -> label pkg (NpmPackageEnv EnvDevelopment)
        _ -> label pkg (NpmPackageEnv EnvProduction)

      traverse_ (label pkg . NpmPackageLocation) depResolved

      -- add edges to required packages
      case depRequires of
        Nothing -> pure ()
        Just required ->
          void $ Map.traverseWithKey (\reqName reqVer -> edge pkg (NpmPackage reqName reqVer)) required

      -- add dependency nodes
      case depDependencies of
        Nothing -> pure ()
        Just deps ->
          void $ Map.traverseWithKey addDep deps

    toDependency :: NpmPackage -> Set NpmPackageLabel -> Dependency
    toDependency pkg = foldr addLabel (start pkg)

    addLabel :: NpmPackageLabel -> Dependency -> Dependency
    addLabel (NpmPackageEnv env) = insertEnvironment env
    addLabel (NpmPackageLocation loc) = insertLocation loc

    start :: NpmPackage -> Dependency
    start NpmPackage{..} =
      Dependency
        { dependencyType = NodeJSType
        , dependencyName = pkgName
        , dependencyVersion = Just $ CEq pkgVersion
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
