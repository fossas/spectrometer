{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Elixir.MixTree (
  PackageName (..),
  MixDep (..),
  MixDepResolved (..),
  DepManager (..),
  DepSCM (..),

  -- * Parsers
  mixTreeCmdOutputParser,
  mixDepsCmdOutputParser,
  parseConstraintExpr,

  -- * Graphs and Analyzers
  buildGraph,
  analyze',
) where

import Control.Effect.Diagnostics hiding (fromMaybe)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Foldable (asum)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing)
import Data.String.Conversion (ToText (toText), toString)
import Data.Text (Text, toLower)
import Data.Text qualified as T
import Data.Void (Void)
import DepTypes
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser)
import Graphing (Graphing, unfold)
import Path
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, takeWhileP, try),
  Parsec,
  anySingle,
  choice,
  chunk,
  empty,
  many,
  manyTill,
  optional,
  parse,
  sepBy,
  some,
  takeP,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char, eol, newline, string)
import Text.Megaparsec.Char.Lexer qualified as L

missingDepVersionsMsg :: Text
missingDepVersionsMsg = "Some of dependencies versions were not resolved from `mix deps` and `mix deps.tree`. Has `mix deps.get` and `mix deps.compile` or `mix compile` been executed?"

analyze' :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Graphing Dependency)
analyze' dir = do
  -- Get all dependencies
  depsAllEnvTree <- context "Identifying relationship among dependencies" $ execParser mixTreeCmdOutputParser dir $ mixDepTreeCmd (Nothing)
  depsAllResolved <- context "Inferring dependencies versioning" $ execParser mixDepsCmdOutputParser dir mixDepCmd

  -- Reminder to get and compile dependencies, if not already done so.
  _ <- if missingResolvedVersions depsAllResolved then context missingDepVersionsMsg $ pure () else pure ()
  context "Building dependency graph" $ pure (buildGraph depsAllEnvTree depsAllResolved M.empty)

-- | Supported Env atoms of mix
data MixEnvAtom = Prod | Test | Dev deriving (Show, Eq, Ord)

-- | Name of the Pkg
newtype PackageName = PackageName {unPackageName :: Text} deriving (Show, Eq, Ord)

-- | Build Manager (:mix, :rebar, :make, :rebar3)
data DepManager = Mix | Rebar3 | Rebar | Make deriving (Eq, Show, Ord)

-- | Source Control Manager (SCM) - e.g. git, hex
data DepSCM = Hex | Git Text (Maybe Text) | Other deriving (Eq, Show, Ord)

-- | Mix Dependency (Unresolved - i.e. without versioning)
data MixDep = MixDep
  { depName :: PackageName
  , depVersion :: Maybe VerConstraint
  , depLocation :: DepSCM
  , subDeps :: [MixDep]
  }
  deriving (Eq, Ord, Show)

-- | Mix Dependency (Resolved - i.e. resolved scm, lock ref, and version)
data MixDepResolved = MixDepResolved
  { depResolvedName :: PackageName
  , depResolvedVersion :: Maybe VerConstraint
  , depResolvedSCM :: DepSCM
  , depResolvedRef :: Maybe VerConstraint
  , depResolvedManager :: Maybe DepManager
  }
  deriving (Show, Ord, Eq)

-- | mix deps --all
mixDepCmd :: Command
mixDepCmd =
  Command
    { cmdName = "mix"
    , cmdArgs = ["deps", "--all"]
    , cmdAllowErr = Never
    }

-- | mix deps.tree --format plain (--only applied to env atom if any)
mixDepTreeCmd :: Maybe MixEnvAtom -> Command
mixDepTreeCmd env =
  Command
    { cmdName = "mix"
    , cmdArgs = case env of
        Nothing -> ["deps.tree", "--format", "plain"]
        Just e -> ["deps.tree", "--format", "plain", "--only", toLower $ toText (show e)]
    , cmdAllowErr = Never
    }

-- | Parses Dep build manager
parseDepManager :: Parser DepManager
parseDepManager =
  choice
    [ Mix <$ string "mix"
    , Rebar3 <$ string "rebar3"
    , Rebar <$ string "rebar"
    , Make <$ string "make"
    ]

-- | Parses Source Control Manager
parseDepSCM :: Parser DepSCM
parseDepSCM = (try parseDepHex <|> parseDepSCMGit)
  where
    parseDepHex :: Parser DepSCM
    parseDepHex = do
      _ <- chunk "Hex package"
      pure (Hex)

    parseDepSCMGit :: Parser DepSCM
    parseDepSCMGit = do
      uri <- chunk "http://" <|> chunk "https://" <|> chunk "git://"
      addr <- takeWhileP (Just "git address") (\c -> c /= ' ' && c /= ')')

      reference <- optional . try $ do
        _ <- chunk " - "
        git_ref <- T.pack <$> some (alphaNumChar <|> char '.')
        return (git_ref)

      pure (Git (uri <> addr) (reference))

type Parser = Parsec Void Text

-- | True if a version is not resolved in `MixDepResolved`, otherwise False.
-- This can happen, if dependencies are not retrieved or compiled.
missingResolvedVersions :: M.Map PackageName MixDepResolved -> Bool
missingResolvedVersions mdr = any isNothing (depResolvedVersion <$> map snd (M.toList mdr))

-- Parses `mix deps` output
mixDepsCmdOutputParser :: Parser (M.Map PackageName MixDepResolved)
mixDepsCmdOutputParser = M.fromList <$> (parseDep `sepBy` char '*') <* eof
  where
    findName :: Parser Text
    findName = takeWhileP (Just "dep") (/= ' ')

    findVersion :: Parser Text
    findVersion = T.pack <$> some (alphaNumChar <|> char '.' <|> char '-')

    parseDep :: Parser (PackageName, MixDepResolved)
    parseDep = do
      _ <- chunk "* " <|> chunk " "

      -- pkg name
      name <- findName
      _ <- chunk " "

      -- If dependencies are not resolved, version won't be printed
      -- This can occur if lock file is missing
      version <- optional . try $ do
        v <- findVersion
        _ <- chunk " "
        return (CEq v)

      -- Source of pkg - e.g. git, hex
      _ <- chunk "("
      scm <- parseDepSCM
      _ <- chunk ")"

      -- If dependencies are not resolved, build manager won't be printed
      -- This can occur if lock file is missing.
      manager <- optional . try $ do
        _ <- chunk " ("
        m <- parseDepManager
        _ <- chunk ")"
        return (m)

      _ <- newline

      -- If dependencies are not resolved, locked reference won't be printed
      ref <- optional . try $ do
        _ <- chunk "  locked at "
        CEq <$> findVersion

      -- Ignore status line, and rest until next entry
      _ <- takeWhileP (Just "ignored") (/= '*')
      pure (PackageName name, MixDepResolved (PackageName name) version scm ref manager)

-- | Parses mix deps.tree raw output
-- First line is always project name, ignore, and parse rest of the tree
mixTreeCmdOutputParser :: Parser [MixDep]
mixTreeCmdOutputParser = manyTill anySingle newline *> mixTreeParser

-- | Parses dependency tree generated by mix deps.tree
mixTreeParser :: Parser [MixDep]
mixTreeParser = concat <$> ((try (mixDep 0) <|> ignoredLine) `sepBy` eol) <* eof
  where
    isEndLine :: Char -> Bool
    isEndLine '\n' = True
    isEndLine '\r' = True
    isEndLine _ = False

    -- ignore content until the end of the line
    ignored :: Parser ()
    ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

    ignoredLine :: Parser [MixDep]
    ignoredLine = do
      ignored
      pure []

    findName :: Parser Text
    findName = takeWhileP (Just "dependency name") (/= ' ')

    findRequirement :: Parser Text
    findRequirement = takeWhile1P (Just "requirements") (/= '(')

    mixDep :: Int -> Parser [MixDep]
    mixDep depth = do
      _ <- takeP (Just "spacing ~ one of (`), (|), ( )") (1 + depth * 4)
      _ <- chunk "-- "
      dep <- findName

      _ <- chunk " "
      version <- optional . try $ do findRequirement

      _ <- chunk "("
      location <- parseDepSCM
      _ <- chunk ")"

      _ <- takeWhileP (Just "ignored") (not . isEndLine)
      deps <- many $ try $ mixRecurse $ depth + 1
      pure [MixDep (PackageName dep) (toDependencyVersion =<< version) location (concat deps)]

    mixRecurse :: Int -> Parser [MixDep]
    mixRecurse depth = do
      _ <- chunk "\n"
      mixDep depth

buildGraph :: [MixDep] -> (M.Map PackageName MixDepResolved) -> (M.Map PackageName [DepEnvironment]) -> Graphing Dependency
buildGraph deps depsResolved depsEnvs = unfold deps subDeps toDependency
  where
    toDependency md =
      Dependency
        { dependencyType = if (depLocation md) == Hex then HexType else GitType
        , dependencyName = dName md depsResolved
        , dependencyVersion = dVersion md depsResolved
        , dependencyLocations = []
        , dependencyEnvironments = fromMaybe [] $ M.lookup (depName md) depsEnvs
        , dependencyTags = M.empty
        }

    dName :: MixDep -> M.Map PackageName MixDepResolved -> Text
    dName m dr = fromMaybe (reqsName) (resolvedGitUrl)
      where
        resolvedGitUrl :: Maybe Text
        resolvedGitUrl = case depResolvedSCM <$> M.lookup (depName m) dr of
          Just (Git url _) -> Just url
          _ -> Nothing

        reqsName :: Text
        reqsName = case depLocation m of
          Git url _ -> url
          _ -> unPackageName $ depName m

    -- Favors in order of locked reference, resolved version, and requirement constraint
    dVersion :: MixDep -> M.Map PackageName MixDepResolved -> Maybe VerConstraint
    dVersion m dr =
      asum
        [ M.lookup (depName m) dr >>= depResolvedVersion
        , M.lookup (depName m) dr >>= depResolvedRef
        , rawVersion
        ]
      where
        rawVersion = case (depLocation m) of
          Git _ (Just ref) -> Just $ CEq ref
          _ -> depVersion m

-- | Operators used in mix requirements
data MixConstraintOperators
  = GreaterThanOrEqual
  | GreaterThan
  | LessThanOrEqual
  | LessThan
  | Equal
  | NotEqual
  | WildcardAny
  | Compatible

toDependencyVersion :: Text -> Maybe VerConstraint
toDependencyVersion dt = case parse parseConstraintExpr "" dt of
  Left _ -> Nothing
  Right v -> case v of
    CEq "*" -> Nothing
    vc -> Just vc

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

-- | Parses `VerConstraint`
parseVerConstraint :: Parser VerConstraint
parseVerConstraint = do
  operator <- whitespaceOrTab *> parseConstraintOperator <* whitespaceOrTab
  versionText <- findVersionText <* whitespaceOrTab
  case operator of
    (Equal) -> return $ CEq versionText
    (NotEqual) -> return $ CNot versionText
    (GreaterThanOrEqual) -> return $ CGreaterOrEq versionText
    (GreaterThan) -> return $ CGreater versionText
    (LessThanOrEqual) -> return $ CLessOrEq versionText
    (LessThan) -> return $ CLess versionText
    (Compatible) -> return $ CCompatible versionText
    (WildcardAny) -> return $ CEq "*"
  where
    findVersionText :: Parser Text
    findVersionText = T.pack <$> some (alphaNumChar <|> char '.' <|> char '-' <|> char '*' <|> char '+')

-- | Parse mix constraint operator
parseConstraintOperator :: Parser MixConstraintOperators
parseConstraintOperator = fromMaybe Equal <$> optional (asum (map symbol operatorList) >>= textToMixVersion)
  where
    operatorList = [">=", "<=", ">", "<", "==", "!=", "~>", "="] :: [Text]

-- | Constructs `MixConstraintOperators` from textual representation (==, >=, etc.) of operators.
textToMixVersion :: (MonadFail m) => Text -> m MixConstraintOperators
textToMixVersion = \case
  "==" -> pure Equal
  "!=" -> pure NotEqual
  ">=" -> pure GreaterThanOrEqual
  "<=" -> pure LessThanOrEqual
  ">" -> pure GreaterThan
  "<" -> pure LessThan
  "~>" -> pure Compatible
  "*" -> pure WildcardAny
  "=" -> pure Equal
  other -> fail ("Could not recognize mix constraint operator: (" <> toString other <> ")")

-- | Parses [mix constraint expression](https://hexdocs.pm/elixir/1.12/Version.html)
parseConstraintExpr :: Parser VerConstraint
parseConstraintExpr = makeExprParser parseVerConstraint operatorTable
  where
    operatorTable :: [[Operator Parser VerConstraint]]
    operatorTable =
      [ [binary "or" COr]
      , [binary "and" CAnd]
      ]
      where
        binary :: Text -> (VerConstraint -> VerConstraint -> VerConstraint) -> Operator Parser VerConstraint
        binary name f = InfixL (f <$ symbol name)