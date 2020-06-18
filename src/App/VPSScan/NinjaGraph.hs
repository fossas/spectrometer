{-# language OverloadedStrings #-}

module App.VPSScan.NinjaGraph
(
  ninjaGraphMain
, NinjaGraphCmdOpts(..)
) where

import Prologue

import Control.Carrier.Error.Either
import Control.Carrier.Trace.Printing
import qualified Data.Text as T
import Effect.ReadFS
import System.Exit (exitFailure)

import App.VPSScan.Types

data NinjaGraphCmdOpts = NinjaGraphCmdOpts
  { depsCmdBasedir :: FilePath
  , depsCmdNinjaGraphOpts :: NinjaGraphOpts
  } deriving Generic

ninjaGraphMain :: NinjaGraphCmdOpts -> IO ()
ninjaGraphMain NinjaGraphCmdOpts{..} = do
  result <- runError @ReadFSErr $ runTrace $ runReadFSIO $ scanNinjaDeps depsCmdBasedir depsCmdNinjaGraphOpts
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
  => FilePath -> NinjaGraphOpts -> m ()
scanNinjaDeps _baseDir NinjaGraphOpts{..} = do
  trace $ "reading ninja deps from " ++ ninjaGraphNinjaPath
  path <- liftIO $ parseAbsFile ninjaGraphNinjaPath
  contents <- readContentsText path
  let ninjaDeps = parseNinjaDeps contents
  let numDeps = length ninjaDeps
  trace $ "found " ++ (show numDeps) ++ " targets"
  trace $ show $ take 10 ninjaDeps

parseNinjaDeps :: Text -> [DepsTarget]
parseNinjaDeps ninjaDepsLines =
  results
  where
    nLines = T.lines ninjaDepsLines
    (_, results) = foldl parseNinjaLine ("starting", []) nLines

parseNinjaLine :: ((Text, [DepsTarget]) -> Text -> (Text, [DepsTarget]))
parseNinjaLine (state, targets) line =
  case state of
    "starting" ->
      if line == "Starting ninja..." then
        ("parsing", [])
      else
        ("starting", [])
    "parsing" ->
      actuallyParseLine line targets
    -- This should never happen
    _ -> ("error", targets)

actuallyParseLine :: Text -> [DepsTarget] -> (Text, [DepsTarget])
-- ignore empty lines
actuallyParseLine "" targets =
  ("parsing", targets)

actuallyParseLine line []
-- error if you're trying to add a dependency and there are no targets yet
-- or if you reach the end of the file and no targets have been found
  | T.isPrefixOf " " line || T.isInfixOf "build completed successfully" line =
    ("error", [])
-- Add the first target
  | otherwise =
    ("parsing", [newDepsTarget])
  where
    newDepsTarget = targetFromLine line

actuallyParseLine line (currentDepsTarget:restOfDepsTargets)
-- ignore the "build completed successfully" line at the end of the file
  | T.isInfixOf "build completed successfully" line =
    ("parsing", (currentDepsTarget:restOfDepsTargets))
-- lines starting with a space add a new dep to the current target
  | T.isPrefixOf " " line =
    ("parsing", (updatedDepsTarget:restOfDepsTargets))
-- lines starting with a non-blank char are new targets
  | otherwise =
    ("parsing", (newDepsTarget:currentDepsTarget:restOfDepsTargets))
  where
    newDepsTarget = targetFromLine line
    updatedDepsTarget = addDepToDepsTarget currentDepsTarget line

targetFromLine :: Text -> DepsTarget
targetFromLine line =
  DepsTarget (T.unpack tar) [] [] Nothing
  where
    (tar, _) = T.breakOn ": #deps" line

addDepToDepsTarget :: DepsTarget -> Text -> DepsTarget
addDepToDepsTarget target line =
  target { dependencies = (newDep:currentDeps)}
  where
    currentDeps = dependencies target
    newDep = parseDepLine line

parseDepLine :: Text -> DepsDependency
parseDepLine line =
  DepsDependency (T.unpack path) componentName hasDeps
  where
    path = T.strip line
    componentName = Nothing -- TODO: get component name
    hasDeps = T.isPrefixOf "out/" path