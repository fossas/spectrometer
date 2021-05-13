{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.Compatibility
  ( compatibilityMain,
    argumentParser,
    Argument,
  )
where

import App.Fossa.EmbeddedBinary (BinaryPaths, toExecutablePath, withCLIv1Binary)
import Control.Effect.Lift (sendIO)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Foldable (traverse_)
import Data.Text (Text, pack)
import Data.Text.Lazy.Encoding
import Effect.Exec (AllowErr (Never), CmdFailure (cmdFailureStdout), Command (..), cmdFailureStderr, exec, runExecIO)
import Effect.Logger (Pretty (pretty), Severity (SevInfo), logInfo, logSticky, withLogger)
import Options.Applicative (Parser, argument, help, metavar, str)
import Path
import System.Exit (exitFailure, exitSuccess)

type Argument = Text

argumentParser :: Parser Argument
argumentParser = pack <$> argument str (metavar "ARGS" <> help "arguments to fossa v1 analyze")

compatibilityMain ::
  [Argument] ->
  IO ()
compatibilityMain args = withLogger SevInfo . runExecIO . withCLIv1Binary $ \v1Bin -> do
  logSticky "[ Waiting for fossa analyze completion ]"
  cmd <- exec [reldir|.|] $ v1Command v1Bin $ args
  logSticky ""

  case cmd of
    Left err -> do
      traverse_ (\accessor -> logInfo . pretty . decodeUtf8 $ accessor err) [cmdFailureStderr, cmdFailureStdout]
      sendIO exitFailure
    Right out -> sendIO (BL.putStr out >> exitSuccess)

v1Command :: BinaryPaths -> [Text] -> Command
v1Command bin args =
  Command
    { cmdName = pack . toFilePath $ toExecutablePath bin,
      cmdArgs = "analyze" : args,
      cmdAllowErr = Never
    }
