module Strategy.Dart.PubDeps (
  analyzeDepsCmd,

  -- * for testing
  dartPubDepCmd,
  depsCmdOutputParser,
  flutterPubDepCmd,
  buildGraph,
  PubDepPackage (..),
) where

import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, toList)
import Data.Set qualified as Set
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (Dependency (..), VerConstraint (CEq))
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser)
import Effect.Logger (Logger (..))
import Effect.ReadFS (Has, ReadFS, readContentsYaml)
import GHC.Generics (Generic)
import Graphing (Graphing, deep, edge, empty, promoteToDirect)
import Path
import Strategy.Dart.PubSpecLock (
  PackageName (..),
  PubDepSource (..),
  PubLockContent (..),
  PubLockPackageMetadata (..),
  isSupported,
  logIgnoredPackages,
  toDependency,
 )
import Text.Megaparsec (
  MonadParsec (eof),
  Parsec,
  anySingle,
  between,
  chunk,
  many,
  optional,
  sepBy,
  skipManyTill,
  (<|>),
 )
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, newline, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Types (GraphBreadth (..))

type Parser = Parsec Void Text

symbol :: Text -> Parser Text
symbol = L.symbol scn

scn :: Parser ()
scn = L.space space1 Megaparsec.empty Megaparsec.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

-- | Represents `dart pub deps -s compact`.
dartPubDepCmd :: Command
dartPubDepCmd =
  Command
    { cmdName = "dart"
    , cmdArgs = ["pub", "deps", "-s", "compact"]
    , cmdAllowErr = Never
    }

-- | Represents `flutter pub deps -s compact`.
-- This is when, dart project has flutter app or other Flutter-specific code.
flutterPubDepCmd :: Command
flutterPubDepCmd =
  Command
    { cmdName = "flutter"
    , cmdArgs = ["pub", "deps", "-s", "compact"]
    , cmdAllowErr = Never
    }

-- | Represents `pub deps -s compact`.
-- Standalone pub command is required for dart lang versions below 2.10
-- https://github.com/dart-lang/sdk/blob/master/CHANGELOG.md#pub-1
pubDepJsonCmd :: Command
pubDepJsonCmd =
  Command
    { cmdName = "pub"
    , cmdArgs = ["deps", "-s", "compact"]
    , cmdAllowErr = Never
    }

data PubDepPackage = PubDepPackage
  { pubDepPackageName :: PackageName
  , pubDepPackageVersion :: Maybe VerConstraint
  , pubDepPackageDeps :: Maybe (Set PackageName)
  , pubDepPackageIsDirect :: Bool
  }
  deriving (Generic, Show, Eq, Ord)

-- | Parse package name.
-- Ref: https://dart.dev/tools/pub/pubspec#name
parsePackageName :: Parser PackageName
parsePackageName = PackageName . toText <$> many (alphaNumChar <|> char '_')

-- | Parse package version.
-- Ref: https://dart.dev/tools/pub/pubspec#version
parsePackageVersion :: Parser VerConstraint
parsePackageVersion = CEq . toText <$> many (alphaNumChar <|> char '.' <|> char '-' <|> char '+')

-- | Parses Pub Package Entry.
--  - pkg_name 1.0.0 [pkg_dep_one]
--  - pkg_name 1.0.0
--  - pkg_name 1.0.0 [pkg_dep_one, pkg_dep_two]
parsePubDepPackage :: Bool -> Parser PubDepPackage
parsePubDepPackage isDirectDep = do
  packageName <- lexeme $ symbol "-" *> lexeme parsePackageName
  packageVersion <- lexeme parsePackageVersion

  -- package may not have any dependencies
  packageSubDeps <- optional (between (symbol "[") (symbol "]") (Set.fromList <$> sepBy parsePackageName (symbol " ")))
  pure $ PubDepPackage packageName (Just packageVersion) packageSubDeps isDirectDep

-- | Parses command output.
-- Reference: https://github.com/dart-lang/pub/blob/291705ca0b9632cb945dd39493dd5b9db41b897a/lib/src/command/deps.dart#L176
-- Note, it prints dependencies, dev dependencies, overrides, and transitive dependencies in order.
depsCmdOutputParser :: Parser [PubDepPackage]
depsCmdOutputParser = parseDeps <* eof
  where
    parseDeps = do
      directDeps <- skipManyTill anySingle (symbol "dependencies:") *> many (parsePubDepPackage True)
      devDeps <- optional $ chunk "dev dependencies:" *> newline *> many (parsePubDepPackage True)

      -- Since, this strategy requires lockfile - dependency override information is redundant,
      -- as lock file already produces resolved dependency source.
      _ <- optional $ chunk "dependency overrides:" *> newline *> many (parsePubDepPackage False)
      transitiveDeps <- optional $ chunk "transitive dependencies:" *> newline *> many (parsePubDepPackage False)

      pure $
        directDeps
          ++ fromMaybe [] devDeps
          ++ fromMaybe [] transitiveDeps

isPackageSupported :: PubLockContent -> PackageName -> Bool
isPackageSupported lockContent pkg = maybe False isSupported $ Map.lookup pkg (packages lockContent)

buildGraph :: PubLockContent -> [PubDepPackage] -> Graphing Dependency
buildGraph lockContent pkgs = Graphing.promoteToDirect isDirectDependency graphPackagesWithEdges
  where
    isDirectDependency :: Dependency -> Bool
    isDirectDependency dep = dependencyName dep `elem` (dependencyName . pkgToDependency <$> reportableDirectPackages)

    graphPackagesWithEdges :: Graphing Dependency
    graphPackagesWithEdges = foldr (uncurry edge) graphPackagesWithoutEdges edges

    edges :: [(Dependency, Dependency)]
    edges = concatMap edgesOf pkgs

    edgesOf :: PubDepPackage -> [(Dependency, Dependency)]
    edgesOf pkg =
      (pkgToDependency $ pubDepPackageName pkg,) . pkgToDependency
        <$> filter isReportable (toList (fromMaybe Set.empty $ pubDepPackageDeps pkg))

    pkgToDependency :: PackageName -> Dependency
    pkgToDependency pkg = toDependency pkg $ metadataOf pkg

    metadataOf :: PackageName -> PubLockPackageMetadata
    metadataOf pkg =
      Map.findWithDefault
        ( PubLockPackageMetadata
            { pubLockPackageIsDirect = any (\x -> pubDepPackageName x == pkg) pkgs
            , pubLockPackageSource = PubLockPackageHostedSource Nothing Nothing
            , pubLockPackageVersion = pubDepPackageVersion =<< find (\x -> pubDepPackageName x == pkg) pkgs
            , pubLockPackageEnvironment = []
            }
        )
        pkg
        (packages lockContent)

    graphPackagesWithoutEdges :: Graphing Dependency
    graphPackagesWithoutEdges = foldr deep Graphing.empty depsWithoutEdges

    depsWithoutEdges :: [Dependency]
    depsWithoutEdges =
      map pkgToDependency $
        filter isReportable $
          pubDepPackageName <$> filter (\x -> Set.empty == fromMaybe Set.empty (pubDepPackageDeps x)) pkgs

    isReportable :: PackageName -> Bool
    isReportable = isPackageSupported lockContent

    reportableDirectPackages :: [PackageName]
    reportableDirectPackages = filter isReportable $ pubDepPackageName <$> filter pubDepPackageIsDirect pkgs

-- | Analyze using pub deps command and lockfile.
-- The pub package manager has a command-line interface that works with either the flutter tool or the dart tool.
-- We attempt with flutter command, than dart command, and lastly with pub (for versions below 2.10).
-- Ref: https://dart.dev/tools/pub/cmd.
analyzeDepsCmd ::
  (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) =>
  Path Abs File ->
  Path Abs Dir ->
  m (Graphing Dependency, GraphBreadth)
analyzeDepsCmd lockFile dir = do
  lockContents <- context "Reading pubspec.lock" $ readContentsYaml lockFile
  depsCmdPackages <-
    execParser depsCmdOutputParser dir flutterPubDepCmd
      <||> execParser depsCmdOutputParser dir dartPubDepCmd
      <||> execParser depsCmdOutputParser dir pubDepJsonCmd

  _ <- logIgnoredPackages lockContents

  context "building graphing from pub deps command and pubspec.lock" $
    pure (buildGraph lockContents depsCmdPackages, Complete)
