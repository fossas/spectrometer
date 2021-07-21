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
  toDependencyVersion,

  -- * Graphs and Analyzers
  buildGraph,
  analyze',
) where

import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Foldable (asum)
import Data.Functor (($>), (<&>))
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes (
  DepType (GitType, HexType),
  Dependency (..),
  VerConstraint (
    CAnd,
    CCompatible,
    CEq,
    CGreater,
    CGreaterOrEq,
    CLess,
    CLessOrEq,
    CNot,
    COr
  ),
 )
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser)
import Graphing (Graphing, unfold)
import Path
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, takeWhileP, try),
  Parsec,
  anySingle,
  between,
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
import Text.Megaparsec.Char (alphaNumChar, char, eol, newline, space, string)
import Text.Megaparsec.Char.Lexer qualified as L

missingDepVersionsMsg :: Text
missingDepVersionsMsg = "Some of dependencies versions were not resolved from `mix deps` and `mix deps.tree`. Has `mix deps.get` and `mix deps.compile` or `mix compile` been executed?"

analyze' :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Graphing Dependency)
analyze' dir = do
  -- Get all dependencies
  depsAllEnvTree <- context "Identifying relationship among dependencies" $ execParser mixTreeCmdOutputParser dir mixDepTreeCmd <&> supportedMixDeps
  depsAllResolved <- context "Inferring dependencies versioning" $ execParser mixDepsCmdOutputParser dir mixDepCmd <&> Map.filter (supportedSCM <$> depResolvedSCM)

  -- Reminder to get and compile dependencies, if not already done so.
  _ <- if missingResolvedVersions depsAllResolved then context missingDepVersionsMsg $ pure () else pure ()
  context "Building dependency graph" $ pure $ buildGraph depsAllEnvTree depsAllResolved

-- | Name of the Package.
newtype PackageName = PackageName {unPackageName :: Text} deriving (Show, Eq, Ord)

-- | Build Manager (:mix, :rebar, :make, :rebar3).
data DepManager = Mix | Rebar3 | Rebar | Make | OtherBuildManager Text deriving (Eq, Show, Ord)

-- | Source Control Manager (SCM) - e.g. git, hex.
data DepSCM = Hex | Git Text (Maybe Text) | Other Text deriving (Eq, Show, Ord)

-- | Mix Dependency (Unresolved - i.e. without versioning).
data MixDep = MixDep
  { depName :: PackageName
  , depVersion :: Maybe VerConstraint
  , depSCM :: DepSCM
  , subDeps :: [MixDep]
  }
  deriving (Eq, Ord, Show)

-- | Mix Dependency (Resolved - i.e. resolved scm, lock ref, and version).
data MixDepResolved = MixDepResolved
  { depResolvedName :: PackageName
  , depResolvedVersion :: Maybe VerConstraint
  , depResolvedSCM :: DepSCM
  , depResolvedRef :: Maybe VerConstraint
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

-- | mix deps.tree --format plain.
mixDepTreeCmd :: Command
mixDepTreeCmd =
  Command
    { cmdName = "mix"
    , cmdArgs = ["deps.tree", "--format", "plain", "--only", "prod"]
    , cmdAllowErr = Never
    }

-- | Parses Dep build manager.
parseDepManager :: Parser DepManager
parseDepManager =
  choice
    [ Mix <$ string "mix"
    , Rebar3 <$ string "rebar3"
    , Rebar <$ string "rebar"
    , Make <$ string "make"
    ]

supportedSCM :: DepSCM -> Bool
supportedSCM Hex = True
supportedSCM (Git _ _) = True
supportedSCM (Other _) = False

-- | Gives only mix dependencies that can be reported.
-- It ignores path dependencies, since they cannot be fetched.
supportedMixDeps :: [MixDep] -> [MixDep]
supportedMixDeps = mapMaybe isSupported
  where
    isSupported m =
      if supportedSCM $ depSCM m
        then
          Just
            MixDep
              { depName = depName m
              , depVersion = depVersion m
              , depSCM = depSCM m
              , subDeps = supportedMixDeps (subDeps m)
              }
        else Nothing

-- | Parses Source Control Manager
parseDepSCM :: Parser DepSCM
parseDepSCM = try parseDepHex <|> parseDepSCMGit <|> parseDepSCMOther
  where
    parseDepHex :: Parser DepSCM
    parseDepHex = do
      _ <- chunk "Hex package"
      pure (Hex)

    parseDepSCMGit :: Parser DepSCM
    parseDepSCMGit = do
      uriScheme <- chunk "http://" <|> chunk "https://" <|> chunk "git://"
      uriRest <- takeWhileP (Just "git address") (\c -> c /= ' ' && c /= ')')

      reference <- optional . try $ do
        _ <- chunk " - "
        Text.pack <$> some (alphaNumChar <|> char '.')

      pure $ Git (uriScheme <> uriRest) reference

    parseDepSCMOther :: Parser DepSCM
    parseDepSCMOther = Other <$> takeWhileP (Just "OtherSCM") (/= ')')

type Parser = Parsec Void Text

-- | True if a version is not resolved in `MixDepResolved`, otherwise False.
-- This can happen, if dependencies are not retrieved or compiled.
missingResolvedVersions :: Map PackageName MixDepResolved -> Bool
missingResolvedVersions mdr = any isNothing (depResolvedVersion <$> map snd (Map.toList mdr))

-- Parses `mix deps` output
mixDepsCmdOutputParser :: Parser (Map PackageName MixDepResolved)
mixDepsCmdOutputParser = Map.fromList <$> (parseDep `sepBy` char '*') <* eof
  where
    findName :: Parser Text
    findName = takeWhileP (Just "dep") (/= ' ')

    findVersion :: Parser Text
    findVersion = Text.pack <$> some (alphaNumChar <|> char '.' <|> char '-')

    parseDep :: Parser (PackageName, MixDepResolved)
    parseDep = do
      _ <- (chunk "*" <|> " ") <* space

      -- Package name
      name <- findName <* space

      -- If dependencies are not resolved, version won't be printed
      -- This can occur if lock file is missing
      version <- optional . try $ CEq <$> findVersion

      -- Source control manager of the package - e.g. git, hex
      scm <- space *> between (char '(') (char ')') parseDepSCM

      -- If dependencies are not resolved, build manager won't be printed
      -- This can occur if lock file is missing.
      _ <- optional . try $ space *> between (char '(') (char ')') parseDepManager
      _ <- newline

      -- If dependencies are not resolved, locked reference won't be printed
      ref <- optional . try $ CEq <$> (space *> "locked at" <* space *> findVersion)

      -- Ignore status line, and rest until next entry
      _ <- takeWhileP (Just "ignored") (/= '*')
      pure (PackageName name, MixDepResolved (PackageName name) version scm ref)

-- | Parses mix deps.tree raw output
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

    ignoredLine :: Parser [MixDep]
    ignoredLine = ignored $> []

    ignored :: Parser ()
    ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

    findName :: Parser Text
    findName = takeWhileP (Just "dependency name") (/= ' ')

    findRequirement :: Parser Text
    findRequirement = takeWhile1P (Just "requirements") (/= '(')

    mixDep :: Int -> Parser [MixDep]
    mixDep depth = do
      _ <- takeP (Just "spacing ~ one of (`), (|), ( )") (1 + depth * 4)

      -- Dependency's name
      dep <- chunk "--" *> space *> findName <* space

      -- Dependency's version
      version <- optional . try $ findRequirement

      -- Dependency's scm
      scm <- between (char '(') (char ')') parseDepSCM
      _ <- takeWhileP (Just "ignored") (not . isEndLine)

      -- Deep Dependencies
      deps <- many $ try $ mixRecurse (depth + 1)
      pure [MixDep (PackageName dep) (toDependencyVersion =<< version) scm (concat deps)]

    mixRecurse :: Int -> Parser [MixDep]
    mixRecurse depth = chunk "\n" *> mixDep depth

buildGraph :: [MixDep] -> (Map PackageName MixDepResolved) -> Graphing Dependency
buildGraph deps depsResolved = unfold deps subDeps toDependency
  where
    toDependency md =
      Dependency
        { dependencyType = dType md
        , dependencyName = dName md depsResolved
        , dependencyVersion = dVersion md depsResolved
        , dependencyLocations = []
        , dependencyEnvironments = []
        , dependencyTags = Map.empty
        }

    dType :: MixDep -> DepType
    dType m = case (depSCM m) of
      Git _ _ -> GitType
      _ -> HexType

    dName :: MixDep -> Map PackageName MixDepResolved -> Text
    dName m dr = fromMaybe (reqsName) (resolvedGitUrl)
      where
        resolvedGitUrl :: Maybe Text
        resolvedGitUrl = case depResolvedSCM <$> Map.lookup (depName m) dr of
          Just (Git url _) -> Just url
          _ -> Nothing

        reqsName :: Text
        reqsName = case depSCM m of
          Git url _ -> url
          _ -> unPackageName $ depName m

    -- Favors in order of locked reference, resolved version, and requirement constraint
    dVersion :: MixDep -> Map PackageName MixDepResolved -> Maybe VerConstraint
    dVersion m dr =
      asum
        [ Map.lookup (depName m) dr >>= depResolvedVersion
        , Map.lookup (depName m) dr >>= depResolvedRef
        , rawVersion
        ]
      where
        rawVersion = case (depSCM m) of
          Git _ (Just ref) -> Just $ CEq ref
          _ -> depVersion m

-- | Operators used in the mix requirements
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
  Right (CEq "*") -> Nothing
  Right vc -> Just vc

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
    Equal -> pure $ CEq versionText
    NotEqual -> pure $ CNot versionText
    GreaterThanOrEqual -> pure $ CGreaterOrEq versionText
    GreaterThan -> pure $ CGreater versionText
    LessThanOrEqual -> pure $ CLessOrEq versionText
    LessThan -> pure $ CLess versionText
    Compatible -> pure $ CCompatible versionText
    WildcardAny -> pure $ CEq "*"
  where
    findVersionText :: Parser Text
    findVersionText = Text.pack <$> some (alphaNumChar <|> char '.' <|> char '-' <|> char '*' <|> char '+')

    parseConstraintOperator :: Parser MixConstraintOperators
    parseConstraintOperator = fromMaybe Equal <$> optional (asum (map symbol operatorList) >>= textToMixVersion)
      where
        operatorList = [">=", "<=", ">", "<", "==", "!=", "~>", "="] :: [Text]

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
      other -> fail ("Could not recognize mix constraint operator: (" <> Text.unpack other <> ")")

-- | Parses [mix constraint expression](https://hexdocs.pm/elixir/1.12/Version.html).
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
