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
  trace $ T.unpack contents
  trace "done"

