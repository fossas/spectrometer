{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Strategy.Python.Poetry.PyProject (
  PyProject (..),
  PyProjectPoetry (..),
  PyProjectBuildSystem (..),
  PoetryDependency (..),
  pyProjectCodec,
  pyProjectBuildSystemCodec,
  usesPoetryBuildSystem,
  PyProjectPoetryGitDependency (..),
  PyProjectPoetryPathDependency (..),
  PyProjectPoetryUrlDependency (..),
  PyProjectPoetryDetailedVersionDependency (..),
  parseConstraintExpr,
  getDependencies,
) where

import Data.Foldable (asum)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Void
import DepTypes
import Text.Megaparsec.Char.Lexer qualified as L
import Toml (TomlCodec, (.=))
import Toml qualified

newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Ord, Show)

data PyProject = PyProject
  { pyprojectBuildSystem :: Maybe PyProjectBuildSystem
  , pyprojectPoetry :: Maybe PyProjectPoetry
  }
  deriving (Show, Eq, Ord)

pyProjectCodec :: TomlCodec PyProject
pyProjectCodec =
  PyProject
    <$> Toml.dioptional (Toml.table pyProjectBuildSystemCodec "build-system") .= pyprojectBuildSystem
      <*> Toml.dioptional (Toml.table pyProjectPoetryCodec "tool.poetry") .= pyprojectPoetry

newtype PyProjectBuildSystem = PyProjectBuildSystem
  { buildBackend :: Text
  }
  deriving (Show, Eq, Ord)

pyProjectBuildSystemCodec :: TomlCodec PyProjectBuildSystem
pyProjectBuildSystemCodec =
  PyProjectBuildSystem
    <$> Toml.diwrap (Toml.text "build-backend") .= buildBackend

data PyProjectPoetry = PyProjectPoetry
  { name :: Maybe Text
  , version :: Maybe Text
  , description :: Maybe Text
  , dependencies :: Map Text PoetryDependency
  , devDependencies :: Map Text PoetryDependency
  }
  deriving (Show, Eq, Ord)

data PoetryDependency
  = PoetryTextVersion Text
  | PyProjectPoetryDetailedVersionDependencySpec PyProjectPoetryDetailedVersionDependency
  | PyProjectPoetryGitDependencySpec PyProjectPoetryGitDependency
  | PyProjectPoetryPathDependencySpec PyProjectPoetryPathDependency
  | PyProjectPoetryUrlDependencySpec PyProjectPoetryUrlDependency
  deriving (Show, Eq, Ord)

pyProjectPoetryCodec :: TomlCodec PyProjectPoetry
pyProjectPoetryCodec =
  PyProjectPoetry
    <$> Toml.dioptional (Toml.text "name") .= name
    <*> Toml.dioptional (Toml.text "version") .= version
    <*> Toml.dioptional (Toml.text "description") .= description
    <*> Toml.tableMap Toml._KeyText pyProjectPoetryDependencyCodec "dependencies" .= dependencies
    <*> Toml.tableMap Toml._KeyText pyProjectPoetryDependencyCodec "dev-dependencies" .= devDependencies

pyProjectPoetryDependencyCodec :: Toml.Key -> TomlCodec PoetryDependency
pyProjectPoetryDependencyCodec key =
  Toml.dimatch matchPyProjectPoetryTextVersionDependecySpec PoetryTextVersion (Toml.text key)
    <|> Toml.dimatch matchPyProjectPoetryDetailedVersionDependencySpec PyProjectPoetryDetailedVersionDependencySpec (Toml.table pyProjectPoetryDetailedVersionDependencyCodec key)
    <|> Toml.dimatch matchPyProjectPoetryGitDependencySpec PyProjectPoetryGitDependencySpec (Toml.table pyProjectPoetryGitDependencyCodec key)
    <|> Toml.dimatch matchPyProjectPoetryPathDependencySpec PyProjectPoetryPathDependencySpec (Toml.table pyProjectPoetryPathDependencyCodec key)
    <|> Toml.dimatch matchPyProjectPoetryUrlDependencySpec PyProjectPoetryUrlDependencySpec (Toml.table pyProjectPoetryUrlDependencyCodec key)

matchPyProjectPoetryTextVersionDependecySpec :: PoetryDependency -> Maybe Text
matchPyProjectPoetryTextVersionDependecySpec (PoetryTextVersion version) = Just version
matchPyProjectPoetryTextVersionDependecySpec _ = Nothing

matchPyProjectPoetryDetailedVersionDependencySpec :: PoetryDependency -> Maybe PyProjectPoetryDetailedVersionDependency
matchPyProjectPoetryDetailedVersionDependencySpec (PyProjectPoetryDetailedVersionDependencySpec spec) = Just spec
matchPyProjectPoetryDetailedVersionDependencySpec _ = Nothing

matchPyProjectPoetryGitDependencySpec :: PoetryDependency -> Maybe PyProjectPoetryGitDependency
matchPyProjectPoetryGitDependencySpec (PyProjectPoetryGitDependencySpec spec) = Just spec
matchPyProjectPoetryGitDependencySpec _ = Nothing

matchPyProjectPoetryPathDependencySpec :: PoetryDependency -> Maybe PyProjectPoetryPathDependency
matchPyProjectPoetryPathDependencySpec (PyProjectPoetryPathDependencySpec spec) = Just spec
matchPyProjectPoetryPathDependencySpec _ = Nothing

matchPyProjectPoetryUrlDependencySpec :: PoetryDependency -> Maybe PyProjectPoetryUrlDependency
matchPyProjectPoetryUrlDependencySpec (PyProjectPoetryUrlDependencySpec spec) = Just spec
matchPyProjectPoetryUrlDependencySpec _ = Nothing

newtype PyProjectPoetryDetailedVersionDependency = PyProjectPoetryDetailedVersionDependency
  { poetryDependencyVersion :: Text
  }
  deriving (Show, Eq, Ord)

pyProjectPoetryDetailedVersionDependencyCodec :: TomlCodec PyProjectPoetryDetailedVersionDependency
pyProjectPoetryDetailedVersionDependencyCodec =
  PyProjectPoetryDetailedVersionDependency
    <$> Toml.text "version" .= poetryDependencyVersion

data PyProjectPoetryGitDependency = PyProjectPoetryGitDependency
  { gitUrl :: Text
  , gitBranch :: Maybe Text
  , gitRev :: Maybe Text
  , gitTag :: Maybe Text
  }
  deriving (Show, Eq, Ord)

pyProjectPoetryGitDependencyCodec :: TomlCodec PyProjectPoetryGitDependency
pyProjectPoetryGitDependencyCodec =
  PyProjectPoetryGitDependency
    <$> Toml.text "git" .= gitUrl
    <*> Toml.dioptional (Toml.text "branch") .= gitBranch
    <*> Toml.dioptional (Toml.text "rev") .= gitRev
    <*> Toml.dioptional (Toml.text "tag") .= gitTag
newtype PyProjectPoetryPathDependency = PyProjectPoetryPathDependency
  { sourcePath :: Text
  }
  deriving (Show, Eq, Ord)

pyProjectPoetryPathDependencyCodec :: TomlCodec PyProjectPoetryPathDependency
pyProjectPoetryPathDependencyCodec =
  PyProjectPoetryPathDependency
    <$> Toml.text "path" .= sourcePath

newtype PyProjectPoetryUrlDependency = PyProjectPoetryUrlDependency
  { sourceUrl :: Text
  }
  deriving (Show, Eq, Ord)

pyProjectPoetryUrlDependencyCodec :: TomlCodec PyProjectPoetryUrlDependency
pyProjectPoetryUrlDependencyCodec =
  PyProjectPoetryUrlDependency
    <$> Toml.text "url" .= sourceUrl

-- | Return `true` if PyProject uses poetry build system, `false` otherwise.
usesPoetryBuildSystem :: PyProject -> Bool
usesPoetryBuildSystem project = case pyprojectBuildSystem project of
  Nothing -> False
  bs -> case (bs) of
    Nothing -> False
    Just bb -> (buildBackend bb) == "poetry.core.masonry.api"

poetryDependencyToDependency :: [DepEnvironment] -> Text -> PoetryDependency -> Dependency
poetryDependencyToDependency depEnvs name deps =
  Dependency
    { dependencyType = depType
    , dependencyName = depName
    , dependencyVersion = depVersion
    , dependencyLocations = depLocations
    , dependencyEnvironments = depEnvironment
    , dependencyTags = depTags
    }
  where
    depType = case deps of
      (PoetryTextVersion _) -> PipType
      (PyProjectPoetryDetailedVersionDependencySpec _) -> PipType
      (PyProjectPoetryGitDependencySpec _) -> GitType
      (PyProjectPoetryPathDependencySpec _) -> UserType
      (PyProjectPoetryUrlDependencySpec _) -> URLType

    depName = case deps of
      (PoetryTextVersion _) -> name
      (PyProjectPoetryDetailedVersionDependencySpec _) -> name
      (PyProjectPoetryGitDependencySpec ds) -> gitUrl ds
      (PyProjectPoetryUrlDependencySpec ds) -> sourceUrl ds
      (PyProjectPoetryPathDependencySpec ds) -> sourcePath ds

    depVersion = case deps of
      (PoetryTextVersion ds) -> toDependencyVersion ds
      (PyProjectPoetryDetailedVersionDependencySpec ds) -> toDependencyVersion (poetryDependencyVersion ds)
      (PyProjectPoetryGitDependencySpec ds) -> case asum [gitTag ds, gitRev ds, gitBranch ds] of
        Nothing -> Nothing
        Just version -> Just $ CEq version
      _ -> Nothing

    depEnvironment = depEnvs
    depLocations = []
    depTags = M.empty

toDependencyVersion :: Text -> Maybe VerConstraint
toDependencyVersion dt = case parse parseConstraintExpr "" dt of
  Left _ -> Nothing
  Right v -> case v of
    CEq "*" -> Nothing
    vc -> Just vc

toDependnecy :: [DepEnvironment] -> Map Text PoetryDependency -> Map Text Dependency
toDependnecy depEnvs = M.mapWithKey $ poetryDependencyToDependency depEnvs

getDependencies :: PyProject -> [Dependency]
getDependencies project = filter ignorePython $ map snd allDeps
  where
    -- pyproject typically includes python as dependency that has to be ignored
    ignorePython = (\d -> dependencyName d /= "python")
    allDeps = case pyprojectPoetry project of
      Nothing -> []
      Just pp -> M.toList (prodDeps) ++ M.toList (devDeps)
        where
          prodDeps = toDependnecy [EnvProduction] (dependencies pp)
          devDeps = toDependnecy [EnvDevelopment] (devDependencies pp)

type Parser = Parsec Void Text

symbol :: Text -> Parser Text
symbol = L.symbol sc
  where
    sc :: Parser ()
    sc = L.space (void $ some $ char ' ') empty empty

-- | Consumes whitespace ` ` or tab `\t`
whitespaceOrTab :: Parser Text
whitespaceOrTab = takeWhileP (Just "whitespaceOrTab") isSpaceOrTab
  where
    isSpaceOrTab c = c == ' ' || c == '\t'

-- | Poetry constraint operators
data PoetryConstraintOperators
  = GreaterThanOrEqual
  | GreaterThan
  | LessThanOrEqual
  | LessThan
  | Equal
  | NotEqual
  | WildcardAny
  | MajorCompatible
  | MinorCompatible

-- | Parses `VerConstraint`
parseVerConstraint :: Parser VerConstraint
parseVerConstraint = do
  operator <- whitespaceOrTab *> parseConstraintOperator <* whitespaceOrTab
  versionText <- many (noneOf [andOperatorChar, orOperatorChar] <* whitespaceOrTab)
  case operator of
    (Equal) -> return $ CEq $ fromCharToText versionText
    (NotEqual) -> return $ CNot $ fromCharToText versionText
    (GreaterThanOrEqual) -> return $ CGreaterOrEq $ fromCharToText versionText
    (GreaterThan) -> return $ CGreater $ fromCharToText versionText
    (LessThanOrEqual) -> return $ CLessOrEq $ fromCharToText versionText
    (LessThan) -> return $ CLess $ fromCharToText versionText
    (MajorCompatible) -> return $ CCompatible $ fromCharToText versionText
    (MinorCompatible) -> return $ CCompatible $ fromCharToText versionText
    (WildcardAny) -> return $ CEq "*"
  where
    fromCharToText :: [Char] -> Text
    fromCharToText chars = T.pack chars
    andOperatorChar = ','
    orOperatorChar = '|'

-- | Parse poetry constraint operator
parseConstraintOperator :: Parser PoetryConstraintOperators
parseConstraintOperator = fromMaybe Equal <$> optional (asum (map symbol operatorList) >>= textToPoetryVersion)
  where
    operatorList = [">=", "<=", "~=", ">", "<", "^", "==", "===", "!=", "~", "*", "="] :: [Text]

-- | Constructs `PoetryConstraintOperators` from textual representation (==, >=, etc.) of operators.
textToPoetryVersion :: (MonadFail m) => Text -> m PoetryConstraintOperators
textToPoetryVersion = \case
  "===" -> pure Equal
  "==" -> pure Equal
  "!=" -> pure NotEqual
  ">=" -> pure GreaterThanOrEqual
  "<=" -> pure LessThanOrEqual
  "~=" -> pure MinorCompatible
  "=" -> pure Equal
  ">" -> pure GreaterThan
  "<" -> pure LessThan
  "^" -> pure MajorCompatible
  "~" -> pure MinorCompatible
  "*" -> pure WildcardAny
  other -> fail ("Could not recognize poetry constraint operator: " <> toString other)

-- | Parses [poetry constraint expression](https://python-poetry.org/docs/dependency-specification/)
-- found in Pyproject.toml into `VerConstraint`
parseConstraintExpr :: Parser VerConstraint
parseConstraintExpr = makeExprParser parseVerConstraint operatorTable
  where
    operatorTable :: [[Operator Parser VerConstraint]]
    operatorTable =
      [ [binary "||" COr]
      , [binary "," CAnd]
      ]
      where
        binary :: Text -> (VerConstraint -> VerConstraint -> VerConstraint) -> Operator Parser VerConstraint
        binary name f = InfixL (f <$ symbol name)