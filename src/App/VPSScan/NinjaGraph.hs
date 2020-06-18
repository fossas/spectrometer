module App.VPSScan.NinjaGraph
(
  ninjaGraphMain
, NinjaGraphCmdOpts(..)
) where

import Prologue

import Control.Carrier.Error.Either
import Control.Carrier.Trace.Printing
import Effect.Exec
import qualified Path.IO as P
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Effect.ReadFS
import System.Process.Typed as PROC
import System.Exit (exitFailure, die)

import App.VPSScan.Types

data NinjaGraphCmdOpts = NinjaGraphCmdOpts
  { ninjaCmdBasedir :: FilePath
  , ninjaCmdNinjaGraphOpts :: NinjaGraphOpts
  } deriving Generic

ninjaGraphMain :: NinjaGraphCmdOpts -> IO ()
ninjaGraphMain NinjaGraphCmdOpts{..} = do
  dir <- validateDir ninjaCmdBasedir
  ninjaDepsContents <- runError @ReadFSErr $ runError @ExecErr $ runTrace $ runReadFSIO $ runExecIO $ getNinjaDeps dir ninjaCmdNinjaGraphOpts
  case ninjaDepsContents of
    Right (Right contents) ->
      runTrace $ scanNinjaDeps ninjaCmdNinjaGraphOpts contents
    Right (Left err) -> do
      putStrLn $ "Error" ++ (show err)
      exitFailure
    Left err -> do
      putStrLn $ "Error" ++ (show err)
      exitFailure


-- If the path to an already generated ninja_deps file was passed in (with the --ninjadeps arg), then
-- read that file to get the ninja deps. Otherwise, generate it with
-- NINJA_ARGS="-t deps" make
getNinjaDeps :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m, Has (Error ExecErr) sig m, Has Trace sig m, MonadIO m) => Path Abs Dir -> NinjaGraphOpts -> m ByteString
getNinjaDeps baseDir opts@NinjaGraphOpts{..} = do
  case ninjaGraphNinjaPath of
    Nothing -> BL.toStrict <$> generateNinjaDeps baseDir opts
    Just ninjaPath -> readNinjaDepsFile ninjaPath

readNinjaDepsFile :: (Has Trace sig m, Has ReadFS sig m, Has (Error ReadFSErr) sig m, MonadIO m) => FilePath -> m ByteString
readNinjaDepsFile ninjaPath = do
  trace $ "reading ninja deps from " ++ ninjaPath
  path <- liftIO $ parseAbsFile ninjaPath
  readContentsBS path

generateNinjaDeps :: (Has (Error ExecErr) sig m, MonadIO m) => Path Abs Dir -> NinjaGraphOpts -> m BL.ByteString
generateNinjaDeps baseDir NinjaGraphOpts{..} = do
  (exitcode, stdout, stderr) <- PROC.readProcess (setWorkingDir (fromAbsDir baseDir) (PROC.shell commandString))
  case (exitcode, stdout, stderr) of
    (ExitSuccess, _, _) -> pure stdout
    (_, _, err) -> throwError (CommandFailed "" (T.pack (show err)))
  where
    commandString = case lunchTarget of
      Nothing -> "cd " ++ show baseDir ++ " && NINJA_ARGS=\"-t deps\" make"
      Just lunch ->  "cd " ++ show baseDir ++ "&& source ./build/envsetup.sh && lunch " ++ (T.unpack lunch) ++ " && NINJA_ARGS=\"-t deps\" make"

scanNinjaDeps ::
  ( Has Trace sig m )
  => NinjaGraphOpts -> ByteString -> m ()
scanNinjaDeps NinjaGraphOpts{..} ninjaDepsContents = do
  let ninjaDeps = parseNinjaDeps ninjaDepsContents
  let numDeps = length ninjaDeps
  trace $ "found " ++ (show numDeps) ++ " targets"

parseNinjaDeps :: ByteString -> [DepsTarget]
parseNinjaDeps ninjaDepsLines =
  results
  where
    newLine = BS.head "\n" -- This is gross, but I couldn't get "BS.split '\n' ninjaDepsLines" to work
    nLines = BS.split newLine ninjaDepsLines
    (_, results) = foldl parseNinjaLine ("starting", []) nLines

parseNinjaLine :: ((Text, [DepsTarget]) -> ByteString -> (Text, [DepsTarget]))
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

actuallyParseLine :: ByteString -> [DepsTarget] -> (Text, [DepsTarget])
-- ignore empty lines
actuallyParseLine "" targets =
  ("parsing", targets)

actuallyParseLine line []
-- error if you're trying to add a dependency and there are no targets yet
-- or if you reach the end of the file and no targets have been found
  | BS.isPrefixOf " " line || BS.isInfixOf "build completed successfully" line =
    ("error", [])
-- Add the first target
  | otherwise =
    ("parsing", [newDepsTarget])
  where
    newDepsTarget = targetFromLine line

actuallyParseLine line (currentDepsTarget:restOfDepsTargets)
-- ignore the "build completed successfully" line at the end of the file
  | BS.isInfixOf "build completed successfully" line =
    ("parsing", (currentDepsTarget:restOfDepsTargets))
-- lines starting with a space add a new dep to the current target
  | BS.isPrefixOf " " line =
    ("parsing", (updatedDepsTarget:restOfDepsTargets))
-- lines starting with a non-blank char are new targets
  | otherwise =
    ("parsing", (newDepsTarget:currentDepsTarget:restOfDepsTargets))
  where
    newDepsTarget = targetFromLine line
    updatedDepsTarget = addDepToDepsTarget currentDepsTarget line

targetFromLine :: ByteString -> DepsTarget
targetFromLine line =
  DepsTarget tar [] [] Nothing
  where
    (tar, _) = BS.breakSubstring ": #deps" line

addDepToDepsTarget :: DepsTarget -> ByteString -> DepsTarget
addDepToDepsTarget target line =
  target { dependencies = (newDep:currentDeps)}
  where
    currentDeps = dependencies target
    newDep = parseDepLine line

parseDepLine :: ByteString -> DepsDependency
parseDepLine line =
  DepsDependency path componentName hasDeps
  where
    path = stripBS line
    componentName = Nothing -- TODO: get component name
    hasDeps = BS.isPrefixOf "out/" path

validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- P.resolveDir' dir
  exists <- P.doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")
  pure absolute

stripBS :: ByteString -> ByteString
stripBS s =
  suffixed
  where
    prefixed = case BS.stripPrefix " " s of
      Nothing -> s
      Just stripped -> stripped
    suffixed = case BS.stripSuffix " " prefixed of
      Nothing -> suffixed
      Just stripped2 -> stripped2
