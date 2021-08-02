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
  { dependencies :: Maybe (Map PackageName PubSpecDepSource)
  , devDependencies :: Maybe (Map PackageName PubSpecDepSource)
  , dependenciesOverrides :: Maybe (Map PackageName PubSpecDepSource)
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

{- ORMOLU_DISABLE -}
instance FromJSON PubSpecDepSource where
  parseJSON (Yaml.String s) = pure $ PubSpecDepHostedSource (Just . CEq $ s) Nothing Nothing
  parseJSON (Yaml.Object o) =
    asum
      [ PubSpecDepHostedSource
          . Just . CEq <$> o .: "version"
          <*> o .: "hosted" |> "name"
          <*> o .: "hosted" |> "url"
      , PubSpecDepGitSource
          . Just . CEq <$> o .: "git" |> "ref"
          <*> o .: "git" |> "url"
      , PubSpecDepGitSource Nothing <$> o .: "git"
      , PubSpecDepPathSource <$> o .: "path"
      , PubSpecDepSdkSource <$> o .: "sdk"
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
isSupported (PubSpecDepHostedSource{}) = True
isSupported (PubSpecDepGitSource _ _) = True
isSupported _ = False

buildGraph :: PubSpecContent -> Graphing.Graphing Dependency
buildGraph content = foldr Graphing.direct Graphing.empty allDeps
  where
    deps :: Map PackageName PubSpecDepSource
    deps = fromMaybe Map.empty $ dependencies content

    devDeps :: Map PackageName PubSpecDepSource
    devDeps = fromMaybe Map.empty $ devDependencies content

    -- In pub manifest, dependency can be overriden.
    superseded :: Map PackageName PubSpecDepSource
    superseded = fromMaybe Map.empty $ dependenciesOverrides content

    notSuperseded :: PackageName -> a -> Bool
    notSuperseded key _ = notMember key superseded

    -- Dependency Sources that can be reported - git, pub.
    supportedDeps :: Map PackageName PubSpecDepSource
    supportedDeps = filterWithKey notSuperseded $ Map.filter isSupported deps

    supportedDevDeps :: Map PackageName PubSpecDepSource
    supportedDevDeps = filterWithKey notSuperseded $ Map.filter isSupported devDeps

    supportedSuperseded :: Map PackageName PubSpecDepSource
    supportedSuperseded = Map.filter isSupported superseded

    allDeps :: [Dependency]
    allDeps =
      map (uncurry $ toDependency [EnvProduction]) (toList supportedDeps)
        ++ map (uncurry $ toDependency [EnvDevelopment]) (toList supportedDevDeps)
        ++ map (\(name, source) -> toDependency (getDepEnv name) name source) (toList supportedSuperseded)

    getDepEnv :: PackageName -> [DepEnvironment]
    getDepEnv name
      | member name deps = [EnvProduction]
      | member name devDeps = [EnvDevelopment]
      | otherwise = []

logIgnoredPkgs :: Has Logger sig m => PubSpecContent -> m ()
logIgnoredPkgs specContent = for_ notSupportedPkgsMsgs (logDebug . pretty)
  where
    notSupportedPkgsMsgs :: [Text]
    notSupportedPkgsMsgs = map (<> " : ignored in analyses. Dependency's source is not supported!") notSupportedPkgs

    notSupportedPkgs :: [Text]
    notSupportedPkgs =
      notSupportedOf (devDependencies specContent)
        ++ notSupportedOf (dependencies specContent)
        ++ notSupportedOf (devDependencies specContent)

    notSupportedOf :: Maybe (Map PackageName PubSpecDepSource) -> [Text]
    notSupportedOf deps = unPackageName . fst <$> (toList . notSupported) (fromMaybe Map.empty deps)

    notSupported :: Map k PubSpecDepSource -> Map k PubSpecDepSource
    notSupported = Map.filter (not . isSupported)

analyzePubSpecFile ::
  (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) =>
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyzePubSpecFile specFile = do
  specContent <- context "Reading pubspec.yaml" $ readContentsYaml specFile
  _ <- logIgnoredPkgs specContent
  context "building graphing from pubspec.yaml" $ pure (buildGraph specContent, Partial)
