module Strategy.Elixir.MixExs (
  analyze,
  buildGraph,
  mixExsFileParser,
  MixExsPackage (..),
  PackageName (..),
  DepSCM (..),
) where

import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Data.Foldable (asum, for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (DepEnvironment (..), DepType (GitType, HexType), Dependency (..), VerConstraint (..))
import Effect.Logger (Logger, logDebug)
import Effect.ReadFS (ReadFS, readContentsParser)
import Graphing (Graphing, fromList)
import Path
import Prettyprinter (pretty)
import Strategy.Elixir.ElixirParser (
  ElixirAccessor (..),
  ElixirAtom (..),
  ElixirText (..),
  ElixirValue (..),
  betweenCurlyBrackets,
  betweenSquareBrackets,
  findKeyword,
  findKeywordWithStringValue,
  lexeme,
  parseElixirValue,
  symbol,
 )
import Strategy.Elixir.MixTree (DepSCM (..), PackageName (..), supportedSCM, toDependencyVersion)
import Text.Megaparsec (
  MonadParsec (takeWhileP, try),
  Parsec,
  anySingle,
  sepEndBy,
  skipManyTill,
  (<|>),
 )
import Types (GraphBreadth (..))

type Parser = Parsec Void Text

-- | MixExsDeps represents dependency entry found in `mix.exs` file.
data MixExsPackage = MixExsPackage
  { mixExsPkgName :: PackageName
  , mixExsPkgDepSCM :: DepSCM
  , pkgExsVersionRequirement :: Maybe VerConstraint
  , pkgExsOnlyEnvironment :: [DepEnvironment]
  }
  deriving (Show, Eq, Ord)

mixExsFileParser :: Parser [MixExsPackage]
mixExsFileParser = lexeme prefix *> deps <* symbol "end"
  where
    prefix = skipManyTill anySingle (symbol "defp deps do")
    deps = betweenSquareBrackets (sepEndBy parseMixExsPackage (lexeme $ symbol ","))

-- | Parses any unrecognized value as ElixirValue of type string.
-- This is to avoid exception when parsing non relevant elixir type in deps entry. - e.g float, global variable, etc.
unRecognizedElixirValue :: Parser ElixirValue
unRecognizedElixirValue = (EString <$> ElixirText) . toText <$> takeWhileP (Just "ignored") (\c -> c /= ',' && c /= '}')

-- | Parses Mix.exs deps entry
-- Reference: https://hexdocs.pm/mix/1.12/Mix.Tasks.Deps.html
--
-- We ignore following options,
--
--  * ':app'          because entry-point of the dependency, is irrelevant for scope of dependency graphing.
--  * ':env'          because env of dependency application, is irrelevant for scope of dependency graphing.
--  * ':compile'      because compiler command of the dependency is irrelevant for scope of dependency graphing.
--  * ':optional'     because current project will always include optional dependency (refer to reference).
--  * ':targets'      because we cannot distinguish target the app will be ran. We take cautionary approach, and do not exclude dependency based on targets.
--  * ':override'     because dependency resolution cannot be performed from exs file only. TODO: Resolve this in mix.lock tactic.
--  * ':manager'      because compiler of the dependency is irrelevant for scope of dependency graphing.
--  * ':runtime'      because the dependency can still be started at runtime later, we take cautionary approach, and consider them in analyses.
--  * ':system_env'   because it is irreverent for scope of dependency graphing.
-- 
parseMixExsPackage :: Parser MixExsPackage
parseMixExsPackage = do
  opts <- betweenCurlyBrackets (sepEndBy (try parseElixirValue <|> unRecognizedElixirValue) (lexeme $ symbol ","))
  pkgName <- getPackageName opts
  pkgEnv <- getPackageEnvironments opts
  pure $ MixExsPackage pkgName (getPackageSCM opts) (lookupPackageVersion opts) pkgEnv
  where
    getPackageName :: MonadFail m => [ElixirValue] -> m PackageName
    getPackageName opts = case listToMaybe ([opt | (Atom opt) <- opts]) of
      Nothing -> fail $ "could not identify package name from: " <> show opts
      Just a -> pure $ (PackageName . getAtom) a

    getPackageEnvironments :: MonadFail m => [ElixirValue] -> m [DepEnvironment]
    getPackageEnvironments opts = case findKeyword (\x -> getAccessor x == "only") opts of
      Nothing -> pure []
      Just (Atom (ElixirAtom env)) -> pure [toDepEnv env]
      Just (List envs) -> pure $ toDepEnv <$> [env | (Atom (ElixirAtom env)) <- envs]
      Just unexpectedValue -> fail $ "unexpected type found with only keyword" <> show unexpectedValue

    toDepEnv :: Text -> DepEnvironment
    toDepEnv "dev" = EnvDevelopment
    toDepEnv "prod" = EnvProduction
    toDepEnv "test" = EnvTesting
    toDepEnv "qa" = EnvTesting
    toDepEnv env = EnvOther env

    getPackageSCM :: [ElixirValue] -> DepSCM
    getPackageSCM opts = case findKeywordWithStringValue opts "path" of
      Nothing -> case lookupPackageGitSource opts of
        Nothing -> Hex
        Just url -> Git url $ lookupPackageGitSourceReference opts
      Just a -> Other a

lookupPackageVersion :: [ElixirValue] -> Maybe VerConstraint
lookupPackageVersion opts = case listToMaybe ([opt | (EString opt) <- opts]) of
  Nothing -> Nothing
  Just (ElixirText versionText) -> toDependencyVersion versionText

lookupPackageGitSource :: [ElixirValue] -> Maybe Text
lookupPackageGitSource opts = case findKeywordWithStringValue opts "git" of
  Nothing -> case findKeywordWithStringValue opts "github" of
    -- If :github, as a shortcut is used to refer to git dependency
    -- http scheme, and host value are not provided.
    Just gitRepo -> Just $ "https://github.com/" <> gitRepo <> ".git"
    Nothing -> Nothing
  url -> url

lookupPackageGitSourceReference :: [ElixirValue] -> Maybe Text
lookupPackageGitSourceReference opts = asum $ map (findKeywordWithStringValue opts) ["ref", "branch", "tag"]

buildGraph :: [MixExsPackage] -> Graphing Dependency
buildGraph packages = fromList $ map toDependency $ filter (supportedSCM . mixExsPkgDepSCM) packages
  where
    toDependency :: MixExsPackage -> Dependency
    toDependency pkg =
      Dependency
        { dependencyType = depType pkg
        , dependencyName = depName pkg
        , dependencyVersion = depVersion pkg
        , dependencyLocations = []
        , dependencyEnvironments = pkgExsOnlyEnvironment pkg
        , dependencyTags = Map.empty
        }

    depType :: MixExsPackage -> DepType
    depType pkg = case mixExsPkgDepSCM pkg of
      Git _ _ -> GitType
      _ -> HexType

    depName :: MixExsPackage -> Text
    depName pkg = case mixExsPkgDepSCM pkg of
      Git url _ -> url
      _ -> unPackageName $ mixExsPkgName pkg

    depVersion :: MixExsPackage -> Maybe VerConstraint
    depVersion pkg = case mixExsPkgDepSCM pkg of
      Git _ version -> CEq <$> version
      _ -> pkgExsVersionRequirement pkg

logIgnoredPackages :: Has Logger sig m => [MixExsPackage] -> m ()
logIgnoredPackages pkgs = for_ notSupportedPackagesMsgs (logDebug . pretty)
  where
    notSupportedPackagesMsgs :: [Text]
    notSupportedPackagesMsgs =
      map
        (<> " : ignored in analyses. Dependency's source is not supported!")
        notSupportedPackageNames

    notSupportedPackageNames :: [Text]
    notSupportedPackageNames =
      map (unPackageName . mixExsPkgName) $
        filter (not . supportedSCM . mixExsPkgDepSCM) pkgs

-- | Analyzes mix.exs file, and graphs only direct dependencies.
analyze :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => Path Abs File -> m (Graphing Dependency, GraphBreadth)
analyze mixExsFile = do
  mixExsDeps <- context "parsing mix.exs file" $ readContentsParser mixExsFileParser mixExsFile
  _ <- logIgnoredPackages mixExsDeps
  context "Building dependency graph from mix.exs" $ pure (buildGraph mixExsDeps, Partial)
