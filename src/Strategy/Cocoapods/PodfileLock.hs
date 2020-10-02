module Strategy.Cocoapods.PodfileLock
  ( discover
  , analyze
  , analyze'
  , buildGraph
  , findSections

  , Dep (..)
  , Pod (..)
  , Section (..)
  ) where

import Control.Effect.Diagnostics
import qualified Data.Char as C
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case findFileNamed "Podfile.lock" files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "cocoapods-podfilelock" CocoapodsGroup $ analyze file

  pure WalkContinue

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsParser findSections file

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = buildGraph <$> readContentsParser findSections file

mkProjectClosure :: Path Abs File -> [Section] -> ProjectClosureBody
mkProjectClosure file sections = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph sections
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

newtype PodfilePkg = PodfilePkg { pkgName :: Text }
  deriving (Eq, Ord, Show)

type PodfileGrapher = LabeledGrapher PodfilePkg PodfileLabel

newtype PodfileLabel =
    PodfileVersion Text
  deriving (Eq, Ord, Show)

toDependency :: PodfilePkg -> Set PodfileLabel -> Dependency
toDependency pkg = foldr applyLabel start
  where

  start :: Dependency
  start = Dependency
    { dependencyType = PodType
    , dependencyName = pkgName pkg
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

  applyLabel :: PodfileLabel -> Dependency -> Dependency
  applyLabel (PodfileVersion ver) dep = dep { dependencyVersion = Just (CEq ver) }

buildGraph :: [Section] -> Graphing Dependency
buildGraph sections = run . withLabeling toDependency $
  traverse_ addSection sections
  where
  addSection :: Has PodfileGrapher sig m => Section -> m ()
  addSection (DependencySection deps) = traverse_ (direct . PodfilePkg . depName) deps
  addSection (PodSection pods) = traverse_ addSpec pods
  addSection _ = pure ()

  addSpec :: Has PodfileGrapher sig m => Pod -> m ()
  addSpec pod = do
    let pkg = PodfilePkg (podName pod)
    -- add edges between spec and specdeps
    traverse_ (edge pkg . PodfilePkg . depName) (podSpecs pod)
    -- add a label for version
    label pkg (PodfileVersion (podVersion pod))

type Parser = Parsec Void Text

data Section =
      PodSection [Pod]
      | DependencySection [Dep]
      | SpecRepos [Remote]
      | ExternalSources [SourceDep]
      | CheckoutOptions [SourceDep]
      | UnknownSection Text
      deriving (Eq, Ord, Show)

newtype Dep = Dep
      { depName :: Text
      } deriving (Eq, Ord, Show)

data SourceDep = SourceDep
      { sDepName :: Text
      , tags     :: Map Text Text
      } deriving (Eq, Ord, Show)

data Pod = Pod
     { podName    :: Text
     , podVersion :: Text
     , podSpecs   :: [Dep]
     } deriving (Eq, Ord, Show)

data Remote = Remote
     { remoteLocation :: Text
     , remoteDeps     :: [Dep]
     } deriving (Eq, Ord, Show)

findSections :: Parser [Section]
findSections = manyTill (try podSectionParser <|> try dependenciesSectionParser <|> try specRepoParser <|> try externalSourcesParser <|> try checkoutOptionsParser <|> unknownSection) eof

unknownSection :: Parser Section
unknownSection = do
  scn
  line <- restOfLine
  pure $ UnknownSection line

podSectionParser :: Parser Section
podSectionParser = sectionParser "PODS:" PodSection podParser

dependenciesSectionParser :: Parser Section
dependenciesSectionParser = sectionParser "DEPENDENCIES:" DependencySection depParser

specRepoParser :: Parser Section
specRepoParser = sectionParser "SPEC REPOS:" SpecRepos remoteParser

externalSourcesParser :: Parser Section
externalSourcesParser = sectionParser "EXTERNAL SOURCES:" ExternalSources externalDepsParser

checkoutOptionsParser :: Parser Section
checkoutOptionsParser = sectionParser "CHECKOUT OPTIONS:" CheckoutOptions externalDepsParser

sectionParser :: Text -> ([a] -> Section) -> Parser a -> Parser Section
sectionParser sectionName lambda parser = nonIndented $ indentBlock $ do
  _ <- chunk sectionName
  return (L.IndentMany Nothing (pure . lambda) parser)

externalDepsParser :: Parser SourceDep
externalDepsParser = indentBlock $ do
  name <- lexeme (takeWhileP (Just "external dep parser") (/= ':'))
  _ <- restOfLine
  return (L.IndentMany Nothing (\exDeps -> pure $ SourceDep name $ M.fromList exDeps) tagParser)

tagParser :: Parser (Text, Text)
tagParser = do
  _ <- chunk ":"
  tag <- lexeme (takeWhileP (Just "tag parser") (/= ':'))
  _ <- chunk ": "
  value <- restOfLine
  pure (tag, value)

remoteParser :: Parser Remote
remoteParser = indentBlock $ do
  location <- restOfLine
  pure (L.IndentMany Nothing (\deps -> pure $ Remote (T.dropWhileEnd (==':') location) deps) depParser)

podParser :: Parser Pod
podParser = indentBlock $ do
  _ <- chunk "- "
  name <- findDep
  version <- findVersion
  _ <- restOfLine
  pure (L.IndentMany Nothing (\deps -> pure $ Pod name version deps) depParser)

depParser :: Parser Dep
depParser = do
  _ <- chunk "- "
  name <- findDep
  _ <- restOfLine
  pure $ Dep name

findDep :: Parser Text
findDep = lexeme (takeWhile1P (Just "dep") (not . C.isSpace))

findVersion :: Parser Text
findVersion = between (char '(') (char ')') (lexeme (takeWhileP (Just "version") (/= ')')))

restOfLine :: Parser Text
restOfLine = takeWhileP (Just "ignored") (not . isEndLine)

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _    = False

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

scn :: Parser ()
scn =  L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc =  L.space (void $ some (char ' ')) empty empty
