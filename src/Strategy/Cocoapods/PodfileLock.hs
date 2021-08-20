module Strategy.Cocoapods.PodfileLock (
  analyze',
  buildGraph,
  PodLock (..),
  Dep (..),
  Pod (..),
  Section (..),
  toSections,
) where

import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  run,
 )
import Data.Aeson (FromJSON (parseJSON), (.:))
import Data.Char qualified as C
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.HashMap.Lazy qualified as HM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Void (Void)
import Data.Yaml qualified as Yaml
import DepTypes (DepType (PodType), Dependency (..), VerConstraint (CEq))
import Effect.Grapher (LabeledGrapher, direct, edge, label, withLabeling)
import Effect.ReadFS (ReadFS, readContentsYaml)
import Graphing (Graphing)
import Path
import Text.Megaparsec (MonadParsec (takeWhileP), Parsec, between, empty, parse, some, takeWhile1P)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do
  podfileLock <- toSections <$> readContentsYaml file
  context "Building dependency graph" $ pure (buildGraph podfileLock)

newtype PodfilePkg = PodfilePkg {pkgName :: Text}
  deriving (Eq, Ord, Show)

type PodfileGrapher = LabeledGrapher PodfilePkg PodfileLabel

newtype PodfileLabel
  = PodfileVersion Text
  deriving (Eq, Ord, Show)

toDependency :: PodfilePkg -> Set PodfileLabel -> Dependency
toDependency pkg = foldr applyLabel start
  where
    start :: Dependency
    start =
      Dependency
        { dependencyType = PodType
        , dependencyName = pkgName pkg
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = []
        , dependencyTags = Map.empty
        }

    applyLabel :: PodfileLabel -> Dependency -> Dependency
    applyLabel (PodfileVersion ver) dep = dep{dependencyVersion = Just (CEq ver)}

buildGraph :: [Section] -> Graphing Dependency
buildGraph sections =
  run . withLabeling toDependency $
    traverse_ addSection sections
  where
    addSection :: Has PodfileGrapher sig m => Section -> m ()
    addSection (DependencySection deps) = traverse_ (direct . PodfilePkg . depName) deps
    addSection (PodSection pods) = traverse_ addSpec pods

    addSpec :: Has PodfileGrapher sig m => Pod -> m ()
    addSpec pod = do
      let pkg = PodfilePkg (podName pod)
      -- add edges between spec and specdeps
      traverse_ (edge pkg . PodfilePkg . depName) (podSpecs pod)
      -- add a label for version
      label pkg (PodfileVersion (podVersion pod))

type Parser = Parsec Void Text

data Section
  = PodSection [Pod]
  | DependencySection [Dep]
  deriving (Eq, Ord, Show)

data PodLock = PodLock
  { lockPods :: [Pod]
  , lockDeps :: [Dep]
  }
  deriving (Show, Eq, Ord)

toSections :: PodLock -> [Section]
toSections lockContent =
  [ PodSection $ lockPods lockContent
  , DependencySection $ lockDeps lockContent
  ]

newtype Dep = Dep
  { depName :: Text
  }
  deriving (Eq, Ord, Show)

data SourceDep = SourceDep
  { sDepName :: Text
  , tags :: Map Text Text
  }
  deriving (Eq, Ord, Show)

data Pod = Pod
  { podName :: Text
  , podVersion :: Text
  , podSpecs :: [Dep]
  }
  deriving (Eq, Ord, Show)

data Remote = Remote
  { remoteLocation :: Text
  , remoteDeps :: [Dep]
  }
  deriving (Eq, Ord, Show)

parseName :: Parser Text
parseName = lexeme $ takeWhile1P (Just "dep") (not . C.isSpace)

parseVersion :: Parser Text
parseVersion = between (char '(') (char ')') $ lexeme (takeWhileP (Just "version") (/= ')'))

parseNameAndVersion :: Parser (Text, Text)
parseNameAndVersion = do
  name <- parseName
  version <- parseVersion
  pure (name, version)

instance FromJSON PodLock where
  parseJSON = Yaml.withObject "Podfile.lock content" $ \o -> do
    pods <- o .: "PODS"
    deps <- o .: "DEPENDENCIES"
    pure $ PodLock pods deps

parserPod :: Text -> [Dep] -> Yaml.Parser Pod
parserPod q deps = case parse parseNameAndVersion "" q of
  Left pErr -> fail $ "could not parse pod name and version" <> show pErr
  Right (name, version) -> pure $ Pod name version deps

instance FromJSON Pod where
  parseJSON (Yaml.String p) = parserPod p []
  parseJSON (Yaml.Object o) =
    if null listedValues
      then fail $ "EMPTY deps in PODS: " <> show o
      else do
        let (key, value) = head $ HM.toList o
        parsedValue :: [Dep] <- parseJSON value
        parserPod key parsedValue
    where
      listedValues :: [(Text, Yaml.Value)]
      listedValues = HM.toList o
  parseJSON podErr = fail $ "could not parse: " <> show podErr

instance FromJSON Dep where
  parseJSON (Yaml.String p) = parseDepName p
    where
      parseDepName :: Text -> Yaml.Parser Dep
      parseDepName q = case parse parseName "" q of
        Left pErr -> fail $ "could not parse name of dependency" <> show pErr
        Right name -> pure $ Dep name
  parseJSON depErr = fail $ "could not parse: " <> show depErr

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space (void $ some (char ' ')) empty empty
