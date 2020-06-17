module App.VPSScan.DepsGraph
(
  depsGraphMain
, DepsGraphCmdOpts(..)
) where

import Prologue

import App.VPSScan.Types

data DepsGraphCmdOpts = DepsGraphCmdOpts
  { depsCmdBasedir :: FilePath
  , depsCmdDepsGraphOpts :: DepsGraphOpts
  } deriving Generic

depsGraphMain :: DepsGraphCmdOpts -> IO ()
depsGraphMain DepsGraphCmdOpts{..} = do
  scanNinjaDeps depsCmdBasedir depsCmdDepsGraphOpts

scanNinjaDeps :: FilePath -> DepsGraphOpts -> IO ()
scanNinjaDeps baseDir DepsGraphOpts{..} = do
  putStrLn $ "scanning ninja deps file found at  " ++ depsGraphNinjaPath

