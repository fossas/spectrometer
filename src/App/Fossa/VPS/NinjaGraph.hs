{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.NinjaGraph
  ( ninjaGraphMain,
    NinjaGraphOptions (..),
  )
where

import App.Fossa.ProjectInference (inferProject, mergeOverride)
import App.Types (BaseDir (..), OverrideProject (..), ProjectRevision (..))
import Conduit (ConduitT, encodeUtf8C, mapC, runConduitRes, stderrC, yield, (.|))
import Control.Carrier.Diagnostics (Diagnostics, fatalText, runDiagnostics, withResult)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (unless, void)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Data.Char (isPrint, isSpace)
import Data.Functor (($>))
import Data.Map (Map, fromList)
import Data.Text (Text, cons, pack)
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Effect.Logger (Severity, withLogger)
import Effect.ReadFS (ReadFS, doesFileExist, resolveFile, runReadFSIO)
import Fossa.API.Types (ApiOpts)
import Path (Abs, File, Path, toFilePath)
import System.Exit (exitSuccess)
import Text.Megaparsec (ParsecT, anySingle, chunk, empty, eof, label, many, manyTill, noneOf, optional, runParserT, some, takeWhile1P, takeWhileP, (<|>))
import Text.Megaparsec.Char (eol, letterChar, space1)
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
  sendIO $ print "STARTING PARSE"
  sendIO $
    runConduitRes $
      sourceParser ninjaParser katiNinjaFilePath
        .| mapC (pack . (++ "\n") . show)
        .| encodeUtf8C
        .| stderrC
  -- soongNinja <- parseNinjaFile soongNinjaFilePath
  -- sendIO $ print soongNinja
  -- ~katiNinja <- parseNinjaFile katiNinjaFilePath
  -- sendIO $ print katiNinja
  sendIO $ print "OK!"
  _ <- sendIO exitSuccess

  -- Upload the parsed Ninja files.

  ProjectRevision {..} <- mergeOverride ngoProjectOverride <$> inferProject (unBaseDir ngoAndroidTopDir)
  return ()

-- result <- runDiagnostics $ getAndParseNinjaDeps undefined undefined undefined
-- case result of
--   Left failure -> do
--     sendIO . print $ renderFailureBundle failure
--     sendIO exitFailure
--   Right _ -> pure ()

data NinjaDecl
  = BuildDecl {outputFiles :: [Text], ruleName :: Text, inputFiles :: [Text]}
  | VarDecl {name :: Text, value :: Text}
  | RuleDecl {name :: Text, variables :: Map Text Text}
  deriving (Show)

type ConduitParser o m r = ParsecT Void Text (ConduitT () o m) r

yieldMany :: (Monad m) => ConduitParser o m o -> ConduitParser o m ()
yieldMany parser = do
  result <- optional parser
  case result of
    Just r -> lift (yield r) *> yieldMany parser
    Nothing -> return ()

-- TODO: How do I report parse errors? Can I run the Either from runParseT into a (Has Diagnostics m)?
sourceParser :: (MonadIO m) => ConduitParser o m r -> Path b File -> ConduitT () o m ()
sourceParser parser filepath = do
  contents <- liftIO $ TIO.readFile filepath'
  void $ runParserT parser filepath' contents
  where
    filepath' = toFilePath filepath

-- TODO: resolve variable substitutions.
--
-- Variable rules are simple:
-- - Variables must be defined before they're referenced.
-- - Variable values are substituted when they're read, and then considered static.
-- - Block scoped variables override global scoped variables.
--
-- It should be enough to just carry around a Map Text Text of variable names and values.
ninjaParser :: (Monad m) => ConduitParser NinjaDecl m ()
ninjaParser = do
  whitespace
  yieldMany $ declarations <* whitespace
  eof
  where
    declarations :: ConduitParser o m NinjaDecl
    declarations = rule <|> build <|> variable

    isHSpace :: Char -> Bool
    isHSpace x = isSpace x && x /= '\n' && x /= '\r'

    hspace1 :: ConduitParser o m ()
    hspace1 = void (takeWhile1P (Just "whitespace") isHSpace) <|> void (chunk "$\n")

    whitespace' :: ConduitParser o m () -> ConduitParser o m ()
    whitespace' s = L.space s (L.skipLineComment "#") empty

    whitespace :: ConduitParser o m ()
    whitespace = dbg' "whitespace" $ label "whitespace" $ whitespace' space1

    trailing :: ConduitParser o m ()
    trailing = dbg' "trailing" $ label "trailing whitespace" $ whitespace' hspace1

    lexeme :: ConduitParser o m a -> ConduitParser o m a
    lexeme = L.lexeme trailing

    symbol :: Text -> ConduitParser o m Text
    symbol = L.symbol trailing

    dbg' :: (Show s) => String -> ConduitParser o m s -> ConduitParser o m s
    dbg' = if False then dbg else const id

    identifier :: ConduitParser o m Text
    identifier =
      dbg' "identifier" $
        label "identifier" $
          lexeme $ do
            start <- letterChar
            rest <- takeWhileP (Just "") (\c -> isPrint c && not (isSpace c))
            return $ start `cons` rest

    value :: ConduitParser o m Text
    value = dbg' "value" $ label "value" $ pack <$> manyTill escaped eol

    escaped :: ConduitParser o m Char
    escaped = escaped' anySingle

    escaped' :: ConduitParser o m Char -> ConduitParser o m Char
    escaped' p =
      (chunk "$\n" $> '\n')
        <|> (chunk "$ " $> ' ')
        <|> (chunk "$:" $> ':')
        <|> (chunk "$$" $> '$')
        <|> p

    variable :: ConduitParser o m NinjaDecl
    variable = dbg' "variable" $ label "variable" (uncurry VarDecl <$> variable')

    variable' :: ConduitParser o m (Text, Text)
    variable' = dbg' "variable'" $
      label "variable" $ do
        name <- identifier
        _ <- symbol "="
        val <- value
        return (name, val)

    indentedVariables :: ConduitParser o m [(Text, Text)]
    indentedVariables = dbg' "vars" $ many $ hspace1 >> variable'

    rule :: ConduitParser o m NinjaDecl
    rule = dbg' "rule" $
      label "rule" $ do
        _ <- symbol "rule "
        name <- identifier
        _ <- eol
        variables <- fromList <$> indentedVariables
        return RuleDecl {..}

    path :: ConduitParser o m Text
    path = dbg' "path" $ label "path" $ pack <$> lexeme (some $ escaped' $ noneOf [' ', '\t', ':', '\r', '\n'])

    -- TODO: parsing for different dependency types (implicit, order-only)
    build :: ConduitParser o m NinjaDecl
    build = dbg' "build" $
      label "build" $ do
        _ <- symbol "build "
        outputFiles <- some path
        _ <- symbol ":"
        ruleName <- identifier
        inputFiles <- manyTill path eol
        _ <- indentedVariables -- TODO: actually use these variables?
        return BuildDecl {..}

-- getAndParseNinjaDeps :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> NinjaGraphOpts -> m ()
-- getAndParseNinjaDeps dir apiOpts ninjaGraphOpts@NinjaGraphOpts {..} = do
--   SherlockInfo {..} <- getSherlockInfo ninjaFossaOpts
--   let locator = createLocator ninjaProjectName sherlockOrgId
--       syOpts = ScotlandYardNinjaOpts locator sherlockOrgId ninjaGraphOpts
--   _ <- uploadBuildGraph apiOpts syOpts graph
--   pure ()
