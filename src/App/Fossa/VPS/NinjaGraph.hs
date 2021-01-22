{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.NinjaGraph
  ( ninjaGraphMain,
    NinjaGraphOptions (..),
  )
where

import App.Fossa.ProjectInference (inferProject, mergeOverride)
import App.Types (BaseDir (..), OverrideProject (..), ProjectRevision (..))
import Conduit (ConduitT, MonadIO (liftIO), decodeUtf8C, encodeUtf8C, foldC, iterMC, mapMC, runConduit, runConduitRes, runResourceT, sourceFile, stderrC, stdoutC, yield, (.|))
import Control.Carrier.Diagnostics (Diagnostics, fatalText, runDiagnostics, withResult)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (unless, void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Char (isSpace)
import Data.Conduit.Combinators (peek)
import Data.Functor (($>))
import Data.Map (Map, fromList)
import Data.Text (Text, pack)
import Data.Void (Void)
import Effect.Logger (Severity, withLogger)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsParser, resolveFile, runReadFSIO)
import Fossa.API.Types (ApiOpts)
import Path (Abs, File, Path, fromAbsFile)
import System.Exit (exitSuccess)
import Text.Megaparsec (Parsec, PosState (..), State (..), anySingle, chunk, defaultTabWidth, empty, eof, initialPos, label, many, manyTill, noneOf, oneOf, some, takeWhile1P, (<|>))
import Text.Megaparsec.Char (alphaNumChar, eol, letterChar, space1)
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

{-

Read chunk boundaries at literal newlines (because comments are until end-of-line)
Match whitespace until failure
- Peek when out of input
- On EOF, succeed

Match decls <* whitespace until failure
> can I do this? will I get partial prefix matches e.g. for indented variables?

StateT here for variable dictionary + "am i in a block?" + "am i in a comment?"

-}
conduitTest :: (MonadIO m) => ConduitT Text Text m ()
conduitTest = do
  x <- peek
  case x of
    Just x' -> do
      liftIO $ print $ "this is the chunk: " <> show x'
      yield x'
    Nothing -> return ()
  where
    loop = undefined
    initialState :: String -> s -> State s e
    initialState name s =
      State
        { stateInput = s,
          stateOffset = 0,
          statePosState =
            PosState
              { pstateInput = s,
                pstateOffset = 0,
                pstateSourcePos = initialPos name,
                pstateTabWidth = defaultTabWidth,
                pstateLinePrefix = ""
              },
          stateParseErrors = []
        }

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
      sourceFile (fromAbsFile soongNinjaFilePath)
        .| decodeUtf8C
        .| conduitTest
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

type Parser = Parsec Void Text

parseNinjaFile :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m [NinjaDecl]
parseNinjaFile = readContentsParser ninja
  where
    ninja :: Parser [NinjaDecl]
    ninja = do
      whitespace
      decls <- many $ declarations <* whitespace
      eof
      return decls

    declarations :: Parser NinjaDecl
    declarations = rule <|> build <|> variable

    isHSpace :: Char -> Bool
    isHSpace x = isSpace x && x /= '\n' && x /= '\r'

    hspace1 :: Parser ()
    hspace1 = void (takeWhile1P (Just "whitespace") isHSpace) <|> void (chunk "$\n")

    whitespace' :: Parser () -> Parser ()
    whitespace' s = L.space s (L.skipLineComment "#") empty

    whitespace :: Parser ()
    whitespace = dbg' "whitespace" $ label "whitespace" $ whitespace' space1

    trailing :: Parser ()
    trailing = dbg' "trailing" $ label "trailing whitespace" $ whitespace' hspace1

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme trailing

    symbol :: Text -> Parser Text
    symbol = L.symbol trailing

    dbg' :: (Show s) => String -> Parser s -> Parser s
    dbg' = if False then dbg else const id

    identifier :: Parser Text
    identifier =
      dbg' "identifier" $
        label "identifier" $
          lexeme $
            pack <$> do
              start <- letterChar
              rest <- many $ alphaNumChar <|> oneOf ['_', '.', '-'] -- TODO: express this as takeWhileP?
              return $ start : rest

    value :: Parser Text
    value = dbg' "value" $ label "value" $ pack <$> manyTill escaped eol

    escaped :: Parser Char
    escaped = escaped' anySingle

    escaped' :: Parser Char -> Parser Char
    escaped' p =
      (chunk "$\n" $> '\n')
        <|> (chunk "$ " $> ' ')
        <|> (chunk "$:" $> ':')
        <|> (chunk "$$" $> '$')
        <|> p

    variable :: Parser NinjaDecl
    variable = dbg' "variable" $ label "variable" (uncurry VarDecl <$> variable')

    variable' :: Parser (Text, Text)
    variable' = dbg' "variable'" $
      label "variable" $ do
        name <- identifier
        _ <- symbol "="
        val <- value
        return (name, val)

    indentedVariables :: Parser [(Text, Text)]
    indentedVariables = dbg' "vars" $ many $ hspace1 >> variable'

    rule :: Parser NinjaDecl
    rule = dbg' "rule" $
      label "rule" $ do
        _ <- symbol "rule "
        name <- identifier
        _ <- eol
        variables <- fromList <$> indentedVariables
        return RuleDecl {..}

    path :: Parser Text
    path = dbg' "path" $ label "path" $ pack <$> lexeme (some $ escaped' $ noneOf [' ', '\t', ':', '\r', '\n'])

    -- TODO: parsing for different dependency types (implicit, order-only)
    build :: Parser NinjaDecl
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
