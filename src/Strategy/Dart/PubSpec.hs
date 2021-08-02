module Strategy.Dart.PubSpec (
  analyzePubSpecFile,

  -- * for testing
  buildGraph,
  PubSpecContent (..),
  PubSpecDepSource (..),
) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Foldable (asum, for_)
import Data.Map (Map, filterWithKey, member, notMember, toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Yaml (FromJSON (parseJSON), (.:), (.:?))
import Data.Yaml qualified as Yaml
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, PubType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Logger (Logger (..), Pretty (pretty), logDebug)
import Effect.ReadFS (Has, ReadFS, readContentsYaml)
import Graphing (Graphing, direct, empty)
import Path
import Strategy.Dart.PubSpecLock (PackageName (..))
import Types (GraphBreadth (..))

data PubSpecContent = PubSpecContent
  { pubSpecDependencies :: Maybe (Map PackageName PubSpecDepSource)
  , pubSpecDevDependencies :: Maybe (Map PackageName PubSpecDepSource)
  , pubSpecDependenciesOverrides :: Maybe (Map PackageName PubSpecDepSource)
  }
  deriving (Show, Eq, Ord)

data PubSpecDepSource
  = PubSpecDepHostedSource
      { version :: Maybe VerConstraint
      , hostedName :: Maybe Text
      , hostedUrl :: Maybe Text
      }
  | PubSpecDepGitSource
      { gitRef :: Maybe VerConstraint
      , gitUrl :: Text
      }
  | PubSpecDepSdkSource {sdkName :: Text}
  | PubSpecDepPathSource {hostPath :: Text}
  deriving (Show, Eq, Ord)

instance FromJSON PubSpecContent where
  parseJSON = Yaml.withObject "pubspec.yaml content" $ \o -> do
    dependencies <- o .:? "dependencies"
    devDependencies <- o .:? "dev_dependencies"
    depOverrides <- o .:? "dependency_overrides"
    pure $ PubSpecContent dependencies devDependencies depOverrides

-- Formatter crashes here. 
{- ORMOLU_DISABLE -}
instance FromJSON PubSpecDepSource where
  parseJSON (Yaml.String s) = pure $ PubSpecDepHostedSource (Just . CEq $ s) Nothing Nothing
  parseJSON (Yaml.Object o) =
    asum
      [ PubSpecDepHostedSource . Just . CEq
          <$> o .: "version"
          <*> o .: "hosted" |> "name"
          <*> o .: "hosted" |> "url"
      , PubSpecDepGitSource . Just . CEq
          <$> o .: "git" |> "ref"
          <*> o .: "git" |> "url"
      , PubSpecDepGitSource Nothing
          <$> o .: "git"
      , PubSpecDepPathSource
          <$> o .: "path"
      , PubSpecDepSdkSource
          <$> o .: "sdk"
      ]
    where
      (|>) :: FromJSON a => Yaml.Parser Yaml.Object -> Text -> Yaml.Parser a
      (|>) parser key = do
        obj <- parser
        obj .: key
  parseJSON _ = fail "failed parsing pub package's source!"
{- ORMOLU_ENABLE -}

toDependency :: [DepEnvironment] -> PackageName -> PubSpecDepSource -> Dependency
toDependency environments name (PubSpecDepHostedSource version _ url) =
  Dependency
    { dependencyType = PubType
    , dependencyName = unPackageName name
    , dependencyVersion = version
    , dependencyLocations = maybeToList url
    , dependencyEnvironments = environments
    , dependencyTags = Map.empty
    }
toDependency environments _ (PubSpecDepGitSource gitRef gitUrl) =
  Dependency
    { dependencyType = GitType
    , dependencyName = gitUrl
    , dependencyVersion = gitRef
    , dependencyLocations = []
    , dependencyEnvironments = environments
    , dependencyTags = Map.empty
    }
toDependency _ _ (PubSpecDepSdkSource _) = error "unexpected sdk dependency conversion"
toDependency _ _ (PubSpecDepPathSource _) = error "unexpected path dependency conversion"

isSupported :: PubSpecDepSource -> Bool
isSupported PubSpecDepHostedSource{} = True
isSupported (PubSpecDepGitSource _ _) = True
isSupported _ = False

buildGraph :: PubSpecContent -> Graphing.Graphing Dependency
buildGraph specContent = foldr Graphing.direct Graphing.empty allDependencies
  where
    dependencies :: Map PackageName PubSpecDepSource
    dependencies = fromMaybe Map.empty $ pubSpecDependencies specContent

    devDependencies :: Map PackageName PubSpecDepSource
    devDependencies = fromMaybe Map.empty $ pubSpecDevDependencies specContent

    -- In pub manifest, dependency can be overriden.
    -- Ref: https://dart.dev/tools/pub/dependencies#dependency-overrides
    supersededDependencies :: Map PackageName PubSpecDepSource
    supersededDependencies = fromMaybe Map.empty $ pubSpecDependenciesOverrides specContent

    notSuperseded :: PackageName -> a -> Bool
    notSuperseded key _ = notMember key supersededDependencies

    -- Dependency Sources that are supported - e.g. git, pub.
    supportedDependencies :: Map PackageName PubSpecDepSource
    supportedDependencies = filterWithKey notSuperseded $ Map.filter isSupported dependencies

    supportedDevDependencies :: Map PackageName PubSpecDepSource
    supportedDevDependencies = filterWithKey notSuperseded $ Map.filter isSupported devDependencies

    supportedSupersededDependencies :: Map PackageName PubSpecDepSource
    supportedSupersededDependencies = Map.filter isSupported supersededDependencies

    allDependencies :: [Dependency]
    allDependencies =
      map (uncurry $ toDependency [EnvProduction]) (toList supportedDependencies)
        ++ map (uncurry $ toDependency [EnvDevelopment]) (toList supportedDevDependencies)
        ++ map (\(name, source) -> toDependency (getDependencyEnv name) name source) (toList supportedSupersededDependencies)

    getDependencyEnv :: PackageName -> [DepEnvironment]
    getDependencyEnv name
      | member name dependencies = [EnvProduction]
      | member name devDependencies = [EnvDevelopment]
      | otherwise = []

logIgnoredPackages :: Has Logger sig m => PubSpecContent -> m ()
logIgnoredPackages specContent = for_ notSupportedPackagesMsgs (logDebug . pretty)
  where
    notSupportedPackagesMsgs :: [Text]
    notSupportedPackagesMsgs = map (<> " : ignored in analyses. Dependency's source is not supported!") notSupportedPackages

    notSupportedPackages :: [Text]
    notSupportedPackages =
      notSupportedOf (pubSpecDevDependencies specContent)
        ++ notSupportedOf (pubSpecDependencies specContent)
        ++ notSupportedOf (pubSpecDependenciesOverrides specContent)

    notSupportedOf :: Maybe (Map PackageName PubSpecDepSource) -> [Text]
    notSupportedOf dependencies = unPackageName . fst <$> (toList . notSupported) (fromMaybe Map.empty dependencies)

    notSupported :: Map k PubSpecDepSource -> Map k PubSpecDepSource
    notSupported = Map.filter $ not . isSupported

analyzePubSpecFile ::
  (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) =>
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyzePubSpecFile specFile = do
  specContent <- context "Reading pubspec.yaml" $ readContentsYaml specFile
  _ <- logIgnoredPackages specContent
  context "building graphing from pubspec.yaml" $ pure (buildGraph specContent, Partial)
