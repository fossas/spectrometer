module Strategy.Crystal.ShardYml (
  buildGraph,
  analyzeShardYmlFile,

  -- * for testing
  ShardYmlContent (..),
  ShardYmlDepSource (..),
  GitSource (..),
  PackageName (..),
) where

import Control.Applicative (Alternative ((<|>)))
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Aeson.Types (FromJSONKey)
import Data.Aeson.Types qualified as AesonTypes
import Data.Foldable (asum)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Yaml (FromJSON (parseJSON), (.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (Has, ReadFS, readContentsYaml)
import Graphing (Graphing, fromList)
import Path
import Types (GraphBreadth (Partial))

newtype PackageName = PackageName {unPackageName :: Text} deriving (Show, Eq, Ord, FromJSONKey)

-- | Represents Shard.yml content
-- Reference: https://github.com/crystal-lang/shards/blob/master/docs/shard.yml.adoc
data ShardYmlContent = ShardYmlContent
  { dependencies :: Maybe (Map PackageName ShardYmlDepSource)
  , devDependencies :: Maybe (Map PackageName ShardYmlDepSource)
  }
  deriving (Show, Eq, Ord)

instance FromJSON PackageName where
  parseJSON (AesonTypes.String pkg) = pure $ PackageName pkg
  parseJSON _ = fail "failed to parse package's name"

data GitSource
  = Github Text
  | Gitlab Text
  | BitBucket Text
  | Git Text
  deriving (Show, Eq, Ord)

data ShardYmlDepSource
  = ShardYamlDepGitSource
      { gitSource :: GitSource
      , gitBranch :: Maybe Text
      , gitTag :: Maybe Text
      , gitCommit :: Maybe Text
      , version :: Maybe Text
      }
  | ShardYamlDepPathSource {hostPath :: Text}
  deriving (Show, Eq, Ord)

instance FromJSON ShardYmlContent where
  parseJSON = Yaml.withObject "shard.yml content" $ \o -> do
    dependencies <- o .:? "dependencies"
    devDependencies <- o .:? "development_dependencies"
    pure $ ShardYmlContent dependencies devDependencies

instance FromJSON ShardYmlDepSource where
  parseJSON (Yaml.Object o) =
    ( ShardYamlDepGitSource <$> gitSource o
        <*> o .:? "branch"
        <*> o .:? "tag"
        <*> o .:? "commit"
        <*> o .:? "version"
    )
      <|> (ShardYamlDepPathSource <$> o .: "path")
    where
      gitSource :: Yaml.Object -> Yaml.Parser GitSource
      gitSource gitO =
        asum
          [ Git <$> gitO .: "git"
          , Github <$> gitO .: "github"
          , Gitlab <$> gitO .: "gitlab"
          , BitBucket <$> gitO .: "bitbucket"
          ]
  parseJSON _ = fail "failed parsing shard dependency's source!"

isSupported :: ShardYmlDepSource -> Bool
isSupported ShardYamlDepGitSource{} = True
isSupported _ = False

buildGraph :: ShardYmlContent -> Graphing Dependency
buildGraph ymlContent = fromList $ prodDeps ++ devDeps
  where
    toDependency :: [DepEnvironment] -> PackageName -> ShardYmlDepSource -> Dependency
    toDependency envs pkgName src =
      Dependency
        { dependencyType = GitType
        , dependencyName = fromMaybe (unPackageName pkgName) $ depName src
        , dependencyVersion = depVersion src
        , dependencyLocations = []
        , dependencyEnvironments = envs
        , dependencyTags = Map.empty
        }

    depName :: ShardYmlDepSource -> Maybe Text
    depName src = case src of
      ShardYamlDepGitSource gitOrigin _ _ _ _ -> case gitOrigin of
        Github repoPath -> Just $ "https://github.com/" <> repoPath <> ".git"
        Gitlab repoPath -> Just $ "https://gitlab.com/" <> repoPath <> ".git"
        BitBucket repoPath -> Just $ "https://bitbucket.com/" <> repoPath <> ".git"
        Git repoUrl -> Just repoUrl
      _ -> Nothing

    depVersion :: ShardYmlDepSource -> Maybe VerConstraint
    depVersion src = case src of
      ShardYamlDepGitSource _ branch tag commit version ->
        asum
          [ CEq <$> version
          , CEq <$> tag
          , CEq <$> branch
          , CEq <$> commit
          ]
      _ -> Nothing

    prodDeps :: [Dependency]
    prodDeps =
      uncurry (toDependency [EnvProduction])
        <$> maybe [] (Map.toList . Map.filter isSupported) (dependencies ymlContent)

    devDeps :: [Dependency]
    devDeps =
      uncurry (toDependency [EnvDevelopment])
        <$> maybe [] (Map.toList . Map.filter isSupported) (devDependencies ymlContent)

analyzeShardYmlFile :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency, GraphBreadth)
analyzeShardYmlFile shardFile = do
  shardContent <- context "Reading shard.yml" $ readContentsYaml shardFile
  context "building graphing from shard.yml only" $ pure (buildGraph shardContent, Partial)
