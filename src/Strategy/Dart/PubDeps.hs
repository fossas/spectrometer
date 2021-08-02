module Strategy.Dart.PubDeps (
  analyzeDepsCmd,

  -- * for testing
  dartPubDepCmd,
  -- parseDepsCmdOutput,
  depsCmdOutputParser,
  flutterPubDepCmd,
  buildGraph,
  PubPkg (..),
  -- DepsCmdOutput (..),
  --   PubDepsCmdOutput(..),
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
  PubLockPkgMetadata (..),
  isSupported,
  logIgnoredPkgs,
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
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Types (GraphBreadth (..))

-- | Represents `dart pub deps -s compact`.
dartPubDepCmd :: Command
dartPubDepCmd =
  Command
    { cmdName = "dart"
    , cmdArgs = ["pub", "deps", "-s", "compact"]
    , cmdAllowErr = Never
    }

-- | Represents `flutter pub deps -s compact`.
flutterPubDepCmd :: Command
flutterPubDepCmd =
  Command
    { cmdName = "flutter"
    , cmdArgs = ["pub", "deps", "-s", "compact"]
    , cmdAllowErr = Never
    }

-- | Represents `pub deps -s compact`.
-- This is to support dart versions below .
pubDepJsonCmd :: Command
pubDepJsonCmd =
  Command
    { cmdName = "pub"
    , cmdArgs = ["deps", "-s", "compact"]
    , cmdAllowErr = Never
    }

data PubPkg = PubPkg
  { pubPkgName :: PackageName
  , pubPkgVersion :: Maybe VerConstraint
  , pubPkgDeps :: Maybe (Set PackageName)
  , isDirect :: Bool
  , isDev :: Bool
  }
  deriving (Generic, Show, Eq, Ord)

type Parser = Parsec Void Text

symbol :: Text -> Parser Text
symbol = L.symbol scn

scn :: Parser ()
scn = L.space space1 Megaparsec.empty Megaparsec.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

parsePkgName :: Parser PackageName
parsePkgName = PackageName . toText <$> many (alphaNumChar <|> char '_')

parsePkgVersion :: Parser VerConstraint
parsePkgVersion = CEq . toText <$> many (alphaNumChar <|> char '_' <|> char '.' <|> char '-' <|> char '+')

parsePubPkg :: Bool -> Bool -> Parser PubPkg
parsePubPkg _isDirect _isDev = do
  _ <- lexeme $ symbol "-"
  packageName <- lexeme parsePkgName
  packageVersion <- lexeme parsePkgVersion
  packageSubDeps <- optional (between (symbol "[") (symbol "]") (Set.fromList <$> (sepBy parsePkgName (symbol " "))))
  _ <- optional $ chunk "\n"
  pure $ PubPkg packageName (Just packageVersion) packageSubDeps _isDirect _isDev

depsCmdOutputParser :: Parser [PubPkg]
depsCmdOutputParser = parseDeps <* eof
  where
    parseDeps = do
      _ <- skipManyTill anySingle (symbol "dependencies:")
      directDeps <- many (parsePubPkg True False)
      devDeps <- optional $ chunk "dev dependencies:" *> chunk "\n" *> many (parsePubPkg True True)
      _ <- optional $ chunk "dependency overrides:" *> chunk "\n" *> many (parsePubPkg False False)
      transitiveDeps <- optional $ chunk "transitive dependencies:" *> chunk "\n" *> many (parsePubPkg False False)
      pure $ directDeps ++ fromMaybe [] devDeps ++ fromMaybe [] transitiveDeps

isPkgSupported :: PubLockContent -> PackageName -> Bool
isPkgSupported lockContent pkg = maybe False isSupported $ Map.lookup pkg (packages lockContent)

-- | Build edges from pub deps command output.
buildGraph :: PubLockContent -> [PubPkg] -> Graphing Dependency
buildGraph lockContent pkgs = Graphing.promoteToDirect isDirectDependency graphPackagesWithEdges
  where
    metadataOf :: PackageName -> PubLockPkgMetadata
    metadataOf pkg =
      Map.findWithDefault
        ( PubLockPkgMetadata
            { pubLockPkgIsDirect = any (\x -> pubPkgName x == pkg) pkgs
            , pubLockPkgSource = PubLockPkgHostedSource Nothing Nothing
            , pubLockPkgVersion = pubPkgVersion =<< find (\x -> pubPkgName x == pkg) pkgs
            , pubLockPkgEnvironment = []
            }
        )
        pkg
        (packages lockContent)

    isDirectDependency :: Dependency -> Bool
    isDirectDependency dep = (dependencyName dep) `elem` ((dependencyName . pkgToDependency) <$> filter isReportable (map pubPkgName (filter isDirect pkgs)))

    pkgToDependency :: PackageName -> Dependency
    pkgToDependency pkg = toDependency pkg (metadataOf pkg)

    isReportable :: PackageName -> Bool
    isReportable = isPkgSupported lockContent

    depsWithoutEdges :: [Dependency]
    depsWithoutEdges = map pkgToDependency $ filter isReportable $ pubPkgName <$> filter (\x -> Set.empty == fromMaybe Set.empty (pubPkgDeps x)) pkgs

    edgesOf :: PubPkg -> [(Dependency, Dependency)]
    edgesOf pkg = (pkgToDependency $ pubPkgName pkg,) . pkgToDependency <$> filter isReportable (toList (fromMaybe Set.empty $ pubPkgDeps pkg))

    edges :: [(Dependency, Dependency)]
    edges = concatMap edgesOf pkgs

    graphPackagesWithoutEdges :: Graphing Dependency
    graphPackagesWithoutEdges = foldr deep Graphing.empty depsWithoutEdges

    graphPackagesWithEdges :: Graphing Dependency
    graphPackagesWithEdges = foldr (uncurry edge) graphPackagesWithoutEdges edges

-- | Analyze using pub deps command and lockfile.
-- The pub package manager has a command-line interface that works with either the flutter tool or the dart tool.
-- https://dart.dev/tools/pub/cmd.
analyzeDepsCmd ::
  (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) =>
  Path Abs File ->
  Path Abs Dir ->
  m (Graphing Dependency, GraphBreadth)
analyzeDepsCmd lockFile dir = do
  lockContents <- context "Reading pubspec.lock" $ readContentsYaml lockFile
  depsPkgs <-
    execParser depsCmdOutputParser dir flutterPubDepCmd
      <||> execParser depsCmdOutputParser dir dartPubDepCmd
      <||> execParser depsCmdOutputParser dir pubDepJsonCmd
  _ <- logIgnoredPkgs lockContents

  context "building graphing from pub deps --json and pubspec.lock" $ pure (buildGraph lockContents depsPkgs, Complete)
