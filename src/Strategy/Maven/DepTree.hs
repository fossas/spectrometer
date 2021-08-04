module Strategy.Maven.DepTree (
  analyze,
  parseDotGraphs,

  -- * Exported for testing
  DotGraph (..),
  PackageId (..),
) where

import Control.Algebra (Has, run)
import Control.Applicative (some, (<|>))
import Control.Effect.Diagnostics (Diagnostics, context, fatal)
import Control.Effect.Exception (finally)
import Control.Effect.Lift (Lift, sendIO)
import Data.Char (isSpace)
import Data.Foldable (for_)
import Data.String.Conversion (toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import DepTypes (
  DepEnvironment (..),
  DepType (MavenType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.Exec (AllowErr (..), Command (..), Exec, exec)
import Effect.Grapher (direct, edge, evalGrapher)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsParser)
import Graphing (Graphing, gmap)
import Path (Abs, Dir, File, Path, Rel, fromAbsFile, parseRelFile, (</>))
import Path.IO (getTempDir, removeFile)
import System.Random (randomIO)
import Text.Megaparsec (
  Parsec,
  between,
  many,
  takeWhile1P,
  try,
 )
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Types (GraphBreadth (Complete))

-- Construct the Command for running `mvn dependency:tree` correctly.
deptreeCmd :: Maybe (Path Abs File) -> Path Abs File -> Command
deptreeCmd settingsFile outputFile =
  Command
    { cmdName = "mvn"
    , cmdArgs =
        [ -- Run `dependency:tree`. See http://maven.apache.org/plugins/maven-dependency-plugin/usage.html#dependency:tree.
          "dependency:tree"
        , -- Output in DOT format, which is Graphviz's graph format. See https://graphviz.org/doc/info/lang.html.
          "-DoutputType=dot"
        , -- Save output to this file. This must be an absolute path for output
          -- to get saved to a single file. If this is a relative path, then
          -- Maven will instead save output at the relative path in each
          -- module's directory, which is super annoying and not desired.
          --
          -- See https://stackoverflow.com/a/42753822.
          "-DoutputFile=" <> toText (fromAbsFile outputFile)
        , -- Append the output of separate modules in a multi-module build,
          -- instead of overwriting the file. If this is not set, then each
          -- module will overwrite the previous module's results, and the only
          -- dependencies in here will be from the last module that ran.
          --
          -- See https://stackoverflow.com/a/42753822.
          "-DappendOutput=true"
        , -- Don't abort module builds in a multi-module build early when they
          -- can still be built. This allows us to get better results when the
          -- only some modules in a multi-module project successfully build, by
          -- maximizing the number of built modules. If this flag is not set,
          -- then Maven stops building when the first error is encountered. With
          -- this flag set, Maven continues and attempts to build every module
          -- that can still be built (since subsequent modules may not depend on
          -- the failing module.)
          "--fail-at-end"
        ]
          <> maybe [] (\p -> ["--settings " <> toText (fromAbsFile p)]) settingsFile
    , -- `mvn dependency:tree` will exit non-zero when the build of any module
      -- in a multi-module build fails. However, this should not cause an Exec
      -- failure, since we can still get partial results.
      --
      -- TODO: We should emit a warning on non-zero exit.
      cmdAllowErr = Always
    }

analyze ::
  ( Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  m (Graphing Dependency, GraphBreadth)
analyze dir = do
  -- Construct the Maven command invocation.
  --
  -- First, we need to determine whether there exists a `settings.xml` that we
  -- need to use. We only look for this in the root (i.e. next to the `pom.xml`)
  -- that constructed this analysis target.
  --
  -- TODO: For maximum magic, find all `settings.xml`s in the CWD, and try with
  -- all of them until one works?
  settingsFile <- mustRelFile "settings.xml"
  let settingsPath = dir </> settingsFile
  settingsExists <- doesFileExist settingsPath

  -- Second, we need a temporary filepath to write results to.
  tmpDir <- sendIO getTempDir
  rand :: Int <- abs <$> sendIO randomIO
  tmpFileName <- mustRelFile $ "fossa-deptree-" <> show rand <> ".dot"
  let tmp = tmpDir </> tmpFileName

  -- Then we run the command, parse the temporary file's output, and clean up
  -- the temporary file.
  _ <-
    context "Running 'mvn dependency:tree'" $
      exec dir $
        deptreeCmd (if settingsExists then Just settingsPath else Nothing) tmp
  graphs <-
    (context "Parsing 'mvn dependency:tree' output" (readContentsParser parseDotGraphs tmp))
      `finally` sendIO (removeFile tmp)
  pure (buildGraph graphs, Complete)
  where
    mustRelFile :: (Has Diagnostics sig m) => FilePath -> m (Path Rel File)
    mustRelFile f = case parseRelFile f of
      Just f' -> pure f'
      Nothing -> fatal $ toText $ "invalid file name: " <> f

buildGraph :: [DotGraph] -> Graphing Dependency
buildGraph = gmap toDependency . foldMap toGraph

toDependency :: PackageId -> Dependency
toDependency PackageId{artifactName, artifactVersion, buildTag} =
  Dependency
    { dependencyType = MavenType
    , dependencyName = artifactName
    , dependencyVersion = Just $ CEq artifactVersion
    , dependencyLocations = []
    , dependencyEnvironments = maybe [EnvProduction] ((: []) . toBuildTag) buildTag
    , dependencyTags = mempty
    }

toGraph :: DotGraph -> Graphing PackageId
toGraph DotGraph{rootNode, edgeList} = run . evalGrapher $ do
  direct rootNode
  for_ edgeList $ uncurry edge

toBuildTag :: Text -> DepEnvironment
toBuildTag = \case
  "compile" -> EnvProduction
  "test" -> EnvTesting
  other -> EnvOther other

data PackageId = PackageId
  { groupName :: Text
  , artifactName :: Text
  , artifactType :: Text
  , artifactVersion :: Text
  , buildTag :: Maybe Text
  }
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

data DotGraph = DotGraph
  { rootNode :: PackageId
  , edgeList :: [(PackageId, PackageId)]
  }
  deriving (Eq, Ord, Show)

parseDotGraphs :: Parser [DotGraph]
parseDotGraphs = some $ do
  root <- symbol "digraph" *> parseNode
  edgeLists <- enclosed "{" "}" (many $ try parseGraphEntry)
  pure $ DotGraph root edgeLists

parseGraphEntry :: Parser (PackageId, PackageId)
parseGraphEntry = do
  from <- parseNode
  to <- (symbol "->") *> parseNode
  _ <- symbol ";"
  pure (from, to)

parseNode :: Parser PackageId
parseNode = lexeme ((quotedName <|> unquotedName) >>= parseName)
  where
    quotedName = enclosed "\"" "\"" $ takeWhile1P (Just "node name") (/= '\"')
    unquotedName = takeWhile1P (Just "node name") (not . isSpace)

parseName :: forall m. MonadFail m => Text -> m PackageId
parseName input = combine parts
  where
    parts :: [Text]
    parts = Text.splitOn ":" input

    combine :: [Text] -> m PackageId
    combine [a, b, c, d, e] = pure . PackageId a b c d $ Just e
    combine [a, b, c, d] = pure $ PackageId a b c d Nothing
    combine items = fail $ toString $ "invalid identifier: " <> Text.intercalate ":" items

enclosed :: Text -> Text -> Parser a -> Parser a
enclosed open close = between (symbol open) (symbol close)

symbol :: Text -> Parser Text
symbol = Lexer.symbol scn

scn :: Parser ()
scn = Lexer.space space1 (Lexer.skipLineComment "//" <|> Lexer.skipLineComment "#") (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme scn
