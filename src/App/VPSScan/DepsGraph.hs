module App.VPSScan.DepsGraph
(
  depsGraphMain
, DepsGraphCmdOpts(..)
) where

import Prologue

import Control.Carrier.Error.Either
import Control.Carrier.Trace.Printing
import qualified Data.Text as T
import Effect.ReadFS
import System.Exit (exitFailure)

import App.VPSScan.Types

data DepsGraphCmdOpts = DepsGraphCmdOpts
  { depsCmdBasedir :: FilePath
  , depsCmdDepsGraphOpts :: DepsGraphOpts
  } deriving Generic

depsGraphMain :: DepsGraphCmdOpts -> IO ()
depsGraphMain DepsGraphCmdOpts{..} = do
  result <- runError @ReadFSErr $ runTrace $ runReadFSIO $ scanNinjaDeps depsCmdBasedir depsCmdDepsGraphOpts
  case result of
    Left err -> do
      print err
      exitFailure
    Right _ -> pure ()

scanNinjaDeps ::
  ( Has ReadFS sig m
  , Has (Error ReadFSErr) sig m
  , Has Trace sig m
  , MonadIO m)
  => FilePath -> DepsGraphOpts -> m ()
scanNinjaDeps baseDir DepsGraphOpts{..} = do
  trace $ "reading ninja deps from " ++ depsGraphNinjaPath
  path <- liftIO $ parseAbsFile depsGraphNinjaPath
  contents <- readContentsText path
  let ninjaDeps = parseNinjaDeps contents
  trace "done"

data Target = Target
  {
    targetPath :: FilePath
  , dependencies :: [Dependency]
  , firstDependency :: Maybe Dependency
  , targetComponentName :: Maybe Text
  } deriving Generic

data Dependency = Dependency
  { dependencyPath :: FilePath
  , dependencyComponentName :: Maybe Text
  , hasDependencies :: Bool
  } deriving Generic

parseNinjaDeps :: Text -> [Target]
parseNinjaDeps ninjaDepsLines =
  results
  where
    nLines = T.lines ninjaDepsLines
    (_, results) = foldl parseNinjaLine ("starting", []) nLines

parseNinjaLine :: ((Text, [Target]) -> Text -> (Text, [Target]))
parseNinjaLine (state, targets) line =
  case state of
    "starting" ->
      if line == "Starting ninja..." then
        ("parsing", [])
      else
        ("starting", [])
    "parsing" ->
      -- lines starting with spaces are deps lines
      -- lines without spaces are target lines
      if (T.isPrefixOf (T.pack "  ") line) then
        ("parsing", targets)
      else
        ("parsing", (t:targets))
        where
          t = Target (T.unpack line) [] Nothing Nothing

