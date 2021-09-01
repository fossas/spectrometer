module Strategy.Swift.PackageSwift (
  analyzePackageSwift,

  -- * for testing,
  buildGraph,
  parsePackageSwiftFile,
  SwiftPackage (..),
  SwiftPackageDep (..),
  SwiftPackageGitDep (..),
) where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Monad (void)
import Control.Monad.Identity (Identity)
import Data.Foldable (asum)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (DepType (GitType, SwiftType), Dependency (..), VerConstraint (CEq))
import Effect.ReadFS (Has, ReadFS, readContentsParser)
import Graphing (Graphing, directs, induceJust)
import Path
import Text.Megaparsec (
  MonadParsec (takeWhile1P, try),
  Parsec,
  ParsecT,
  anySingle,
  between,
  empty,
  sepEndBy,
  skipManyTill,
 )
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Types (GraphBreadth (..))

-- | Parsing
-- *
type Parser = Parsec Void Text

sc :: Parser ()
sc =
  Lexer.space
    space1
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

scWOComment :: Parser ()
scWOComment = Lexer.space space1 empty empty

symbolWOComment :: Text -> Parser Text
symbolWOComment = Lexer.symbol scWOComment

betweenDoubleQuotes :: Parser a -> Parser a
betweenDoubleQuotes = between (symbol "\"") (symbol "\"")

betweenSquareBrackets :: Parser a -> Parser a
betweenSquareBrackets = between (symbol "[") (symbol "]")

betweenBrackets :: Parser a -> Parser a
betweenBrackets = between (symbol "(") (symbol ")")

maybeComma :: Parser ()
maybeComma = void $ optional $ lexeme $ symbol ","

parseQuotedText :: Parser Text
parseQuotedText = betweenDoubleQuotes (lexeme $ takeWhile1P (Just "quoted text") (/= '"'))

parseKeyValue :: Text -> Parser a -> Parser a
parseKeyValue t parser = lexeme $ symbol (t <> ":") *> parser

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _ = False

-- | Represents https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#methods.
data SwiftPackage = SwiftPackage
  { swiftToolVersion :: Text
  , packageDependencies :: [SwiftPackageDep]
  }
  deriving (Show, Eq, Ord)

-- | Represents https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#package-dependency.
data SwiftPackageDep
  = GitSource SwiftPackageGitDep
  | PathSource Text
  deriving (Show, Eq, Ord)

data SwiftPackageGitDep = SwiftPackageGitDep
  { srcOf :: Text
  , branchOf :: Maybe Text
  , revisionOf :: Maybe Text
  , fromOf :: Maybe Text
  , exactOf :: Maybe Text
  , upToNextMajorOf :: Maybe Text
  , upToNextMinorOf :: Maybe Text
  , closedInterval :: Maybe (Text, Text)
  , rhsHalfOpenInterval :: Maybe (Text, Text)
  }
  deriving (Show, Eq, Ord)

parsePackageDep :: Parser SwiftPackageDep
parsePackageDep = try parsePathDep <|> parseGitDep
  where
    parsePathDep :: Parser SwiftPackageDep
    parsePathDep = PathSource <$> (symbol ".package" *> betweenBrackets (parseKeyValue "path" parseQuotedText))

    parseRequirement :: Text -> Parser Text
    parseRequirement t =
      try (symbol ("." <> t) *> betweenBrackets parseQuotedText)
        <|> parseKeyValue t parseQuotedText

    parseUpToOperator :: Text -> Parser Text
    parseUpToOperator t = symbol ("." <> t) *> betweenBrackets (parseRequirement "from")

    parseRange :: Text -> Parser (Text, Text)
    parseRange rangeOperator = do
      lhs <- parseQuotedText
      _ <- symbol rangeOperator
      rhs <- parseQuotedText
      pure (lhs, rhs)

    optionallyTry :: ParsecT Void Text Identity a -> ParsecT Void Text Identity (Maybe a)
    optionallyTry p = optional . try $ p <* maybeComma

    parseGitDep :: Parser SwiftPackageDep
    parseGitDep = do
      _ <- symbol ".package" <* symbol "("
      _ <- optionallyTry (parseKeyValue "name" parseQuotedText)

      -- Url (Required Field)
      url <- parseKeyValue "url" $ parseQuotedText <* maybeComma

      -- Version Constraint (Optional Fields)
      revision <- optionallyTry $ parseRequirement "revision"
      branch <- optionallyTry $ parseRequirement "branch"
      from <- optionallyTry $ parseRequirement "from"
      exact <- optionallyTry $ parseRequirement "exact"
      upToMajor <- optionallyTry $ parseUpToOperator "upToNextMajor"
      upToMinor <- optionallyTry $ parseUpToOperator "upToNextMinor"
      closedInterval <- optionallyTry $ parseRange "..."
      rhsHalfOpenRange <- optionallyTry $ parseRange "..<"

      _ <- symbol ")"
      pure $
        GitSource $
          SwiftPackageGitDep
            url
            branch
            revision
            from
            exact
            upToMajor
            upToMinor
            closedInterval
            rhsHalfOpenRange

parsePackageDependencies :: Parser [SwiftPackageDep]
parsePackageDependencies = do
  _ <- lexeme $ skipManyTill anySingle $ symbol "let package = Package("
  skipManyTill anySingle (symbol "dependencies:") *> betweenSquareBrackets (sepEndBy (lexeme parsePackageDep) $ symbol ",")

parseSwiftToolVersion :: Parser Text
parseSwiftToolVersion =
  symbolWOComment "//"
    *> parseKeyValue
      "swift-tools-version"
      (takeWhile1P (Just "swift-tools-version") $ not . isEndLine)

parsePackageSwiftFile :: Parser SwiftPackage
parsePackageSwiftFile = do
  -- Package.swift must specify version for swift tools
  -- https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#about-the-swift-tools-version
  swiftToolVersion <- parseSwiftToolVersion
  SwiftPackage swiftToolVersion <$> parsePackageDependencies

-- | Analysis
-- *
analyzePackageSwift :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing.Graphing Dependency, GraphBreadth)
analyzePackageSwift manifestFile = do
  manifestContent <- context "Identifying dependencies in Package.swift" $ readContentsParser parsePackageSwiftFile manifestFile
  graph <- context "Building dependency graph" $ pure $ buildGraph manifestContent
  pure (graph, Partial)

-- | Graph Building
-- *
buildGraph :: SwiftPackage -> Graphing.Graphing Dependency
buildGraph pkg = induceJust $ directs (map toDependency $ packageDependencies pkg)

toDependency :: SwiftPackageDep -> Maybe Dependency
toDependency (GitSource pkgDep) =
  Just $
    Dependency
      { dependencyType = depType
      , dependencyName = srcOf pkgDep
      , dependencyVersion =
          CEq
            <$> asum
              [ branchOf pkgDep
              , revisionOf pkgDep
              , exactOf pkgDep
              , toFromExpression <$> fromOf pkgDep
              , toUpToNextMajorExpression <$> upToNextMajorOf pkgDep
              , toUpToNextMinorExpression <$> upToNextMinorOf pkgDep
              , toClosedIntervalExpression <$> closedInterval pkgDep
              , toRhsHalfOpenIntervalExpression <$> rhsHalfOpenInterval pkgDep
              ]
      , dependencyLocations = []
      , dependencyEnvironments = []
      , dependencyTags = Map.empty
      }
  where
    -- from constraint is equivalent to upToNextMajor
    -- Reference: https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#methods-3
    toFromExpression :: Text -> Text
    toFromExpression = toUpToNextMajorExpression

    -- Fetcher accepts ~ operator, to perform
    -- upToNext minor constraint validation.
    toUpToNextMajorExpression :: Text -> Text
    toUpToNextMajorExpression v = "^" <> v

    -- Fetcher accepts ~ operator, to perform
    -- upToNext minor constraint validation.
    toUpToNextMinorExpression :: Text -> Text
    toUpToNextMinorExpression v = "~" <> v

    toClosedIntervalExpression :: (Text, Text) -> Text
    toClosedIntervalExpression (lhs, rhs) = ">=" <> lhs <> " " <> "<=" <> rhs

    toRhsHalfOpenIntervalExpression :: (Text, Text) -> Text
    toRhsHalfOpenIntervalExpression (lhs, rhs) = ">=" <> lhs <> " " <> "<" <> rhs

    depType :: DepType
    depType =
      if isJust $ asum [branchOf pkgDep, revisionOf pkgDep, exactOf pkgDep]
        then GitType
        else SwiftType
toDependency (PathSource _) = Nothing
