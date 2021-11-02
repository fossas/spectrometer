module Strategy.Perl (
  discover,

  -- * for testing
  PackageName (..),
  PerlMeta (..),
  buildGraph,
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Aeson (Object, ToJSON, decodeFileStrict', eitherDecodeFileStrict')
import Data.Aeson.Types (FromJSONKey, Parser, withObject)
import Data.Aeson.Types qualified as AesonTypes
import Data.Foldable (asum)
import Data.Map (Map, toList)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Yaml (FromJSON (parseJSON), decodeFileEither, (.:), (.:?))
import DepTypes (
  DepEnvironment (..),
  DepType (CpanType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walk',
 )
import Effect.Exec (Has)
import Effect.ReadFS (ReadFS, readContentsJson, readContentsYaml)
import GHC.Generics (Generic)
import Graphing (Graphing, deeps)
import Path
import Path.Extra (extensionOf)
import Text.Read (readMaybe)
import Types (
  DependencyResults (..),
  DiscoveredProject (..),
  GraphBreadth (Partial),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject PerlProject]
discover dir = context "Perl" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [PerlProject]
findProjects = walk' $ \dir _ files -> do
  -- We prefer MYMETA over META.
  -- Reference: https://metacpan.org/dist/App-mymeta_requires/view/bin/mymeta-requires
  case asum $ map (`findFileNamed` files) (["MYMETA.json", "MYMETA.yml", "META.json", "META.yml"]) of
    Nothing -> pure ([], WalkContinue)
    Just f -> pure ([PerlProject dir f], WalkContinue)

data PerlProject = PerlProject
  { perlDir :: Path Abs Dir
  , perlMetaFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PerlProject
instance AnalyzeProject PerlProject where
  analyzeProject _ = getDeps

mkProject :: PerlProject -> DiscoveredProject PerlProject
mkProject project =
  DiscoveredProject
    { projectType = "perl"
    , projectBuildTargets = mempty
    , projectPath = perlDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PerlProject -> m DependencyResults
getDeps project = do
  graph <- analyze (perlMetaFile project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [perlMetaFile project]
      }

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze metaFile = do
  content <- context "Identifying dependencies in meta file" $
    case extensionOf metaFile of
      Just "json" -> readContentsJson metaFile
      _ -> readContentsYaml metaFile

  pure $ buildGraph (content)

newtype PackageName = PackageName {unPackageName :: Text} deriving (Show, Eq, Ord, FromJSONKey)

-- | Represents Metafile for various versions.
-- References:
-- v1.0: http://module-build.sourceforge.net/META-spec-v1.0.html
-- v1.1: http://module-build.sourceforge.net/META-spec-v1.1.html
-- v1.2: http://module-build.sourceforge.net/META-spec-v1.2.html
-- v1.3: http://module-build.sourceforge.net/META-spec-v1.3.html
-- v1.4: https://metacpan.org/release/DAGOLDEN/CPAN-Meta-2.101090
-- v2.0: https://metacpan.org/release/DAGOLDEN/CPAN-Meta-2.150010
data PerlMeta = PerlMeta
  { version :: Double
  , runtimeRequires :: Maybe (Map PackageName (Maybe Text))
  , buildRequires :: Maybe (Map PackageName (Maybe Text))
  , testRequires :: Maybe (Map PackageName (Maybe Text))
  , developRequires :: Maybe (Map PackageName (Maybe Text))
  , configureRequires :: Maybe (Map PackageName (Maybe Text))
  }
  deriving (Generic, Show, Eq, Ord)

instance FromJSON PackageName where
  parseJSON (AesonTypes.String packageName) = pure $ PackageName packageName
  parseJSON _ = fail "failed to parse package's name"

instance FromJSON PerlMeta where
  parseJSON = withObject "meta content" $ \o -> do
    -- spec_version can be either be string or number
    -- in yaml, version is provided as string, where as in json, it is numeric
    specVersion :: Double <-
      (o .: "meta-spec" |> "version")
        <|> ( do
                s <- o .: "meta-spec" |> "version"
                case readMaybe s of
                  Nothing -> fail "not a number"
                  Just x -> pure x
            )

    if specVersion > 1.4
      then parseAboveV1_4 o specVersion
      else parseBelowV1_5 o specVersion
    where
      (|>) :: FromJSON a => Parser Object -> Text -> Parser a
      (|>) parser key = do
        obj <- parser
        obj .: key

      (|?>) :: FromJSON a => Parser (Maybe Object) -> Text -> Parser (Maybe a)
      (|?>) parser key = do
        obj <- parser
        case obj of
          Nothing -> pure Nothing
          Just o -> o .:? key

      -- Reference: https://metacpan.org/release/DAGOLDEN/CPAN-Meta-2.101090
      parseAboveV1_4 obj version = do
        runtimeRequires <- obj .:? "prereqs" |?> "runtime" |?> "requires"
        buildRequires <- obj .:? "prereqs" |?> "build" |?> "requires"
        testRequires <- obj .:? "prereqs" |?> "test" |?> "requires"
        developRequires <- obj .:? "prereqs" |?> "develop" |?> "requires"
        configureRequires <- obj .:? "prereqs" |?> "configure" |?> "requires"

        pure $
          PerlMeta
            version
            runtimeRequires
            buildRequires
            testRequires
            developRequires
            configureRequires

      -- Reference: http://module-build.sourceforge.net/META-spec-v1.4.html
      parseBelowV1_5 obj version = do
        runtimeRequires1 <- obj .:? "requires"
        buildRequires1 <- obj .:? "build_requires"
        configureRequires1 <- obj .:? "configure_requires"
        pure $ PerlMeta version runtimeRequires1 buildRequires1 Nothing Nothing configureRequires1

buildGraph :: PerlMeta -> Graphing Dependency
buildGraph meta =
  deeps $
    filter
      notNamedPerl
      (runtimeDeps ++ testDeps ++ developDeps ++ buildDeps ++ configureDeps)
  where
    runtimeDeps = getDepsOf EnvProduction runtimeRequires
    testDeps = getDepsOf EnvTesting testRequires
    developDeps = getDepsOf EnvDevelopment developRequires
    buildDeps = getDepsOf EnvDevelopment buildRequires
    configureDeps = getDepsOf EnvDevelopment configureRequires

    notNamedPerl :: Dependency -> Bool
    notNamedPerl dep = dependencyName dep /= "perl"

    getDepsOf :: DepEnvironment -> (PerlMeta -> Maybe (Map PackageName (Maybe Text))) -> [Dependency]
    getDepsOf env getter = map (toDependency env) (toList $ fromMaybe mempty (getter meta))

    toDependency :: DepEnvironment -> (PackageName, Maybe Text) -> Dependency
    toDependency env (pkgName, version) =
      Dependency
        { dependencyName = unPackageName pkgName
        , dependencyType = CpanType
        , dependencyVersion = CEq <$> version
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = mempty
        }
