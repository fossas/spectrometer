{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.NinjaGraph
  ( ninjaGraphMain,
    NinjaGraphOptions (..),
  )
where

import App.Types (BaseDir (..), OverrideProject (..))
import Conduit (ConduitT, concatMapAccumC, decodeUtf8C, encodeUtf8C, filterC, mapC, mapMC, runConduitRes, sourceFile, stderrC, (.|))
import Control.Carrier.Diagnostics (Diagnostics, fatalText, runDiagnostics, withResult)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (unless, void)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (isPrint, isSpace)
import Data.Functor (($>))
import Data.Text (unpack, Text, pack)
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
  sendIO $
    runConduitRes $
      sourceFile (toFilePath katiNinjaFilePath)
        .| decodeUtf8C
        .| intoLines
        -- .| iterMC (\c -> liftIO $ putStrLn $ "DEBUG 1: " <> show c <> "\n")
        .| removeComments
        -- .| iterMC (\c -> liftIO $ putStrLn $ "DEBUG 2: " <> show c <> "\n")
        .| escapeNewlines
        -- .| iterMC (\c -> liftIO $ putStrLn $ "DEBUG 3: " <> show c <> "\n")
        .| toStatement (toFilePath katiNinjaFilePath)
        -- .| iterMC debugLeft
        .| filterStatements
        .| mapC (\c -> "NEW CHUNK: " <> pack (show c) <> "\n")
        .| encodeUtf8C
        .| stderrC
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
removeComments :: (Monad m) => ConduitT Text Text m ()
removeComments =
  mapC (fst . T.breakOn "#")
    .| filterC (\s -> not (T.null s) && not (T.all isSpace s))

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
        then (prefix <> s, [])
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
-- TODO: Properly handle errors.
--
-- TODO: For proper error handling, we should thread positional state through
-- each parser, probably using runParser' and mapAccum.
--
-- TODO: Can I report error handling within Diagnostics?
--
-- TODO: Can we hack this to be even faster by just doing dumb prefix checks for
-- statement type before running the parser?
toStatement :: (MonadIO m) => String -> ConduitT Text NinjaStmt m ()
toStatement filename = mapMC parse
  where
    parse contents = case runParser ninjaStmtParser filename contents of
      Right r -> return r
      Left l -> liftIO $ die $ errorBundlePretty l

type Parser = Parsec Void Text

-- TODO: Do variable resolution within this parser. We'll need to thread the
-- accumulated state through.
ninjaStmtParser :: Parser NinjaStmt
ninjaStmtParser = declarations <* whitespace <* eof
  where
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

    rule :: Parser NinjaStmt
    rule = dbg' "rule" $ RuleStmt <$ symbol "rule " <* identifier

    path :: Parser Text
    path = dbg' "path" $ pack <$> lexeme (some $ escaped' $ satisfy (\c -> c /= ' ' && c /= ':'))

    -- TODO: parsing for different dependency types (implicit, order-only)
    build :: Parser NinjaStmt
    build = dbg' "build" $ do
      _ <- symbol "build "
      outputFiles <- some path
      _ <- symbol ":"
      ruleName <- identifier
      inputFiles <- many path
      return BuildStmt {..}

    defaultTarget :: Parser NinjaStmt
    defaultTarget = dbg' "default" $ DefaultStmt <$ symbol "default " <* some identifier

    subNinja :: Parser NinjaStmt
    subNinja = dbg' "subninja" $ SubNinjaStmt <$ symbol "subninja " <* path

    include :: Parser NinjaStmt
    include = dbg' "include" $ SubNinjaStmt <$ symbol "include " <* path

    pool :: Parser NinjaStmt
    pool = dbg' "pool" $ SubNinjaStmt <$ symbol "pool " <* identifier

    variable :: Parser NinjaStmt
    variable = dbg' "variable" $
      label "variable" $ do
        w <- whitespace
        name <- identifier
        _ <- symbol "="
        value <- pack <$> some escaped
        let isIndented = not $ T.null w
        return VarStmt {..}

    declarations :: Parser NinjaStmt
    declarations =
      rule
        <|> build
        <|> defaultTarget
        <|> subNinja
        <|> include
        <|> pool
        <|> variable

-- Filter out all statements we don't care about to avoid doing extra work
-- downstream.
filterStatements :: (Monad m) => ConduitT NinjaStmt NinjaStmt m ()
filterStatements = filterC f
  where
    f BuildStmt{} = True
    f VarStmt{} = True
    f _ = False
