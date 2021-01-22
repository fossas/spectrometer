{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.NinjaGraph
  ( ninjaGraphMain,
    NinjaGraphOptions (..),
  )
where

import App.Types (BaseDir (..), OverrideProject (..))
import Conduit (ConduitT, MonadResource, MonadThrow, concatMapAccumC, decodeUtf8C, encodeUtf8C, filterC, iterMC, mapC, mapMC, runConduitRes, sourceFile, stderrC, (.|))
import Control.Carrier.Diagnostics (Diagnostics, fatalText, runDiagnostics, withResult)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (unless, void)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (isPrint, isSpace)
import Data.Functor (($>))
import Data.Text (Text, isPrefixOf, pack, unpack)
import qualified Data.Text as T
import Data.Void (Void)
import Effect.Logger (Severity, withLogger)
import Effect.ReadFS (ReadFS, doesFileExist, resolveFile, runReadFSIO)
import Fossa.API.Types (ApiOpts)
import Path (Abs, File, Path, toFilePath)
import System.Exit (die, exitSuccess)
import Text.Megaparsec (Parsec, anySingle, chunk, eof, errorBundlePretty, label, many, runParser, satisfy, some, takeWhile1P, takeWhileP, (<|>))
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

data NinjaGraphOptions = NinjaGraphOptions
  { -- TODO: These three fields seem fairly common. Factor out into `CommandOptions t`?
    ngoLogSeverity :: Severity,
    ngoAPIOptions :: ApiOpts,
    ngoProjectOverride :: OverrideProject,
    --
    ngoAndroidTopDir :: BaseDir,
    ngoLunchCombo :: Text,
    ngoScanID :: Text,
    ngoBuildName :: Text
  }

ninjaGraphMain :: NinjaGraphOptions -> IO ()
ninjaGraphMain n@NinjaGraphOptions {..} =
  withLogger ngoLogSeverity $ do
    result <- runReadFSIO $ runDiagnostics $ ninjaGraph n
    withResult ngoLogSeverity result pure

ninjaFileNotFoundError :: Path Abs File -> Text
ninjaFileNotFoundError p =
  pack $
    unlines
      [ "Could not find the generated Ninja build file at " <> show p <> ".",
        "",
        "Debugging checklist:",
        "",
        "- Did you run an Android build?",
        "  If so, this file should be generated. Check if it exists.",
        "",
        "- Is base directory set to the root of the Android build?",
        "  This is the positional argument passed to the CLI. By default, this is the current working directory.",
        "",
        "- Are you using a custom $OUT directory for your Android build?",
        "  This is unsupported. Rename your $OUT directory to \"//out\"."
      ]

debugLeft :: (MonadIO m) => Either Text a -> m ()
debugLeft e = case e of
  Right _ -> return ()
  Left l -> liftIO $ putStrLn $ "ERROR:\n" <> unpack l

ninjaGraph :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => NinjaGraphOptions -> m ()
ninjaGraph NinjaGraphOptions {..} = withLogger ngoLogSeverity $ do
  -- Resolve the Ninja files from the Android repository's root.
  let (BaseDir top) = ngoAndroidTopDir
  soongNinjaFilePath <- resolveFile top "out/soong/build.ninja"
  katiNinjaFilePath <- resolveFile top $ "out/build-" <> ngoLunchCombo <> ".ninja"

  -- Check if the files exist.
  ok <- doesFileExist soongNinjaFilePath
  unless ok $ fatalText $ ninjaFileNotFoundError soongNinjaFilePath
  ok' <- doesFileExist katiNinjaFilePath
  unless ok' $ fatalText $ ninjaFileNotFoundError katiNinjaFilePath

  -- Parse the Ninja files.
  sendIO $ putStrLn "STARTING PARSE"
  -- sendIO $
  --   runConduitRes $
  --     sourceParser ninjaParser katiNinjaFilePath
  --       .| encodeUtf8C
  --       .| stderrC
  -- sendIO $ runConduitRes $ parseNinjaConduit soongNinjaFilePath
  sendIO $ runConduitRes $ parseNinjaConduit katiNinjaFilePath
  -- soongNinja <- parseNinjaFile soongNinjaFilePath
  -- sendIO $ print soongNinja
  -- ~katiNinja <- parseNinjaFile katiNinjaFilePath
  -- sendIO $ print katiNinja
  sendIO $ putStrLn "OK!"
  _ <- sendIO exitSuccess

  -- Upload the parsed Ninja files.

  -- ProjectRevision {..} <- mergeOverride ngoProjectOverride <$> inferProject (unBaseDir ngoAndroidTopDir)
  return ()

-- result <- runDiagnostics $ getAndParseNinjaDeps undefined undefined undefined
-- case result of
--   Left failure -> do
--     sendIO . print $ renderFailureBundle failure
--     sendIO exitFailure
--   Right _ -> pure ()

-- getAndParseNinjaDeps :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> NinjaGraphOpts -> m ()
-- getAndParseNinjaDeps dir apiOpts ninjaGraphOpts@NinjaGraphOpts {..} = do
--   SherlockInfo {..} <- getSherlockInfo ninjaFossaOpts
--   let locator = createLocator ninjaProjectName sherlockOrgId
--       syOpts = ScotlandYardNinjaOpts locator sherlockOrgId ninjaGraphOpts
--   _ <- uploadBuildGraph apiOpts syOpts graph
--   pure ()

parseNinjaConduit :: (MonadResource m, MonadThrow m) => Path b File -> ConduitT () Void m ()
parseNinjaConduit filePath =
  sourceFile (toFilePath filePath)
    .| decodeUtf8C
    -- .| iterMC (\c -> liftIO $ putStrLn $ "DEBUG 0: " <> show c <> "\n")
    .| intoLines
    -- .| iterMC (\c -> liftIO $ putStrLn $ "DEBUG 1: " <> show c <> "\n")
    .| removeCommentsAndEmptyLines
    -- .| iterMC (\c -> liftIO $ putStrLn $ "DEBUG 2: " <> show c <> "\n")
    .| escapeNewlines
    -- .| iterMC (\c -> liftIO $ putStrLn $ "DEBUG 3: " <> show c <> "\n")
    .| toStatement (toFilePath filePath)
    -- .| iterMC debugLeft
    .| filterStatements
    -- TODO: Fold over statements to resolve variable substitutions.
    -- TODO: Upload results to API.
    .| mapC (\c -> "NEW CHUNK: " <> pack (show c) <> "\n")
    .| encodeUtf8C
    .| stderrC

-- Split chunks into lines (delimited by "\n"). When a chunk does not end in a
-- newline, hold on to it and add it to the beginning of the next chunk.
intoLines :: (Monad m) => ConduitT Text Text m ()
intoLines = concatMapAccumC split ""
  where
    split :: Text -> Text -> (Text, [Text])
    split s leftover =
      if T.last s == '\n'
        then ("", ls)
        else (last ls, init ls)
      where
        ls = T.lines $ leftover <> s

-- Remove line comments. Anything starting from "#" and going to the end of the
-- line is a comment.
--
-- Throw away lines that are now empty or all-whitespace (because they were
-- entirely comment) rather than emit empty chunks.
--
-- HACK: Apparently, "#" characters on the RHS of a variable assignment
-- don't count as comments! This lovely and undocumented contradiction of
-- Ninja's manual is handled as a hack here, because handling it correctly
-- (i.e. by parsing variable values and using that to determine whether any
-- particular "#" is or is not a comment) is way too annoying.
--
-- HACK 2: Apparently, "#"s that are part of file paths don't count either!
-- It is now 3:31 AM. I have noticed that the generated files never seem to
-- add comments that are not at the beginning of the line. I am going to
-- cross my fingers and just roll with that. To verify for yourself:
-- `rg '[^# ]#' out/soong/build.ninja | less`.
removeCommentsAndEmptyLines :: (Monad m) => ConduitT Text Text m ()
removeCommentsAndEmptyLines = filterC $ \l -> not (T.null l) && (T.head l /= '#')

-- Handle escaped newlines. The "$\n" sequence escapes newlines. Since we split
-- on "\n" earlier (and we've already removed comments, so we don't need to
-- worry about commented-out escape sequences), any chunk ending in "$" was
-- actually an escaped newline (it would have been "$\n"), so that chunk should
-- be merged with the next chunk.
escapeNewlines :: (Monad m) => ConduitT Text Text m ()
escapeNewlines = concatMapAccumC escapeN ""
  where
    escapeN :: Text -> Text -> (Text, [Text])
    escapeN s prefix =
      if T.last s == '$'
        then (prefix <> T.init s, [])
        else ("", [prefix <> s])

data NinjaStmt
  = BuildStmt {outputFiles :: [Text], ruleName :: Text, inputFiles :: [Text]}
  | VarStmt {name :: Text, value :: Text, isIndented :: Bool}
  | RuleStmt -- We don't care about kinds of statements below.
  | DefaultStmt
  | SubNinjaStmt
  | IncludeStmt
  | PoolStmt
  deriving (Show)

-- Parse each line into a statement. Conveniently, all statements are one line
-- (after handling newline escapes, which we did in an earlier step).
--
-- This uses a gross prefix-matching hack for statements for performance. Hooray
-- for simple syntaxes!
--
-- TODO: Properly handle errors.
--
-- For proper error handling, we should thread positional state through
-- each parser, probably using runParser' and mapAccum.
--
-- Can I report error handling within Diagnostics?
--
-- TODO: Do variable resolution within the parsers. We'll need to thread an
-- accumulated state through.
toStatement :: (MonadIO m) => String -> ConduitT Text NinjaStmt m ()
toStatement filename = mapMC parse
  where
    parse contents
      | "rule " `isPrefixOf` contents = return RuleStmt
      | "default " `isPrefixOf` contents = return DefaultStmt
      | "subninja " `isPrefixOf` contents = return SubNinjaStmt
      | "include " `isPrefixOf` contents = return IncludeStmt
      | "pool " `isPrefixOf` contents = return PoolStmt
      | "build " `isPrefixOf` contents = handleError $ runParser build filename contents
      | otherwise = handleError $ runParser variable filename contents
    handleError result = case result of
      Right r -> return r
      Left l -> liftIO $ die $ errorBundlePretty l

type Parser = Parsec Void Text

dbg' :: (Show s) => String -> Parser s -> Parser s
dbg' s = (if False then dbg s else id) . label s

whitespace :: Parser Text
whitespace = dbg' "whitespace" $ takeWhileP Nothing isSpace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ void whitespace

symbol :: Text -> Parser Text
symbol = L.symbol $ void whitespace

-- TODO: Should this also technically be using escaped?
identifier :: Parser Text
identifier = dbg' "identifier" $ lexeme $ takeWhile1P Nothing (\c -> isPrint c && not (isSpace c))

-- TODO: Can we do this escaping in earlier stages, so we can use
-- takeWhile1P? I think not, because escaping " " and ":" are critical.
escaped' :: Parser Char -> Parser Char
escaped' p =
  (chunk "$ " $> ' ')
    <|> (chunk "$:" $> ':')
    <|> (chunk "$$" $> '$')
    <|> p

escaped :: Parser Char
escaped = escaped' anySingle

path :: Parser Text
path = dbg' "path" $ pack <$> lexeme (some $ escaped' $ satisfy (\c -> c /= ' ' && c /= ':'))

statement :: Parser NinjaStmt -> Parser NinjaStmt
statement p = p <* whitespace <* eof

-- TODO: Parse different dependency type symbols (implicit, order-only).
build :: Parser NinjaStmt
build = dbg' "build" $
  statement $ do
    _ <- symbol "build "
    outputFiles <- some path
    _ <- symbol ":"
    ruleName <- identifier
    inputFiles <- many path
    return BuildStmt {..}

variable :: Parser NinjaStmt
variable = dbg' "variable" $
  statement $ do
    w <- whitespace
    name <- identifier
    _ <- symbol "="
    value <- pack <$> many escaped
    let isIndented = not $ T.null w
    return VarStmt {..}

-- Filter out all statements we don't care about to avoid doing extra work
-- downstream.
--
-- FIXME: This actually doesn't work right - we also have to filter out variable
-- statements scoped to a rule's block, so they don't get mistaken for variables
-- from a preceding build statement's scope.
filterStatements :: (Monad m) => ConduitT NinjaStmt NinjaStmt m ()
filterStatements = filterC f
  where
    f BuildStmt {} = True
    f VarStmt {} = True
    f _ = False
