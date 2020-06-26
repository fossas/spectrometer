module App.VPSScan.NinjaGraph
(
  ninjaGraphMain
, NinjaGraphCmdOpts(..)
, scanNinjaDeps
) where

import Prologue

import Control.Carrier.Trace.Printing
import Control.Carrier.Diagnostics
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Effect.Exec
import Data.Text.Encoding (decodeUtf8)
import Effect.ReadFS
import System.Process.Typed as PROC
import System.Exit (exitFailure)
import Data.Text.Prettyprint.Doc (pretty)
import Network.HTTP.Req
import qualified System.FilePath as FP

import App.VPSScan.Types
import App.Util (validateDir, parseUri)

-- end of copy-paste

data NinjaGraphCmdOpts = NinjaGraphCmdOpts
  { ninjaCmdBasedir :: FilePath
  , ninjaCmdNinjaGraphOpts :: NinjaGraphOpts
  } deriving Generic

data NinjaGraphError = ErrorRunningNinja Text
                     | NoNinjaDepsStartLineFound
                     | NoNinjaDepsEndLineFound
                     | NinjaDepsParseError
  deriving (Eq, Ord, Show, Generic, Typeable)

instance ToDiagnostic NinjaGraphError where
  renderDiagnostic = \case
    ErrorRunningNinja err -> "Error while running Ninja: " <> pretty err
    NoNinjaDepsStartLineFound -> "The output of \"ninja -t deps\" did not contain the \"Starting ninja...\" line"
    NoNinjaDepsEndLineFound   -> "The output of \"ninja -t deps\" did not contain the \"build completed successfully\" line"
    NinjaDepsParseError   -> "There was an error while parsing the output of \"ninja -t deps\""

data NinjaParseState = Starting | Parsing | Complete | Error

ninjaGraphMain :: NinjaGraphCmdOpts -> IO ()
ninjaGraphMain NinjaGraphCmdOpts{..} = do
  dir <- validateDir ninjaCmdBasedir
  result <- runDiagnostics $ getAndParseNinjaDeps dir ninjaCmdNinjaGraphOpts
  case result of
    Left failure -> do
      print $ renderFailureBundle failure
      exitFailure
    Right _ -> pure ()


getAndParseNinjaDeps :: (Has Diagnostics sig m, MonadIO m) => Path Abs Dir -> NinjaGraphOpts -> m ()
getAndParseNinjaDeps dir ninjaGraphOpts = do
  ninjaDepsContents <- runTrace $ runReadFSIO $ runExecIO $ getNinjaDeps dir ninjaGraphOpts
  graph <- scanNinjaDeps ninjaGraphOpts ninjaDepsContents
  _ <- runHTTP $ postDepsGraphResults ninjaGraphOpts graph
  pure ()

-- If the path to an already generated ninja_deps file was passed in (with the --ninjadeps arg), then
-- read that file to get the ninja deps. Otherwise, generate it with
-- NINJA_ARGS="-t deps" make
getNinjaDeps :: (Has ReadFS sig m, Has Diagnostics sig m, Has Trace sig m, MonadIO m) => Path Abs Dir -> NinjaGraphOpts -> m ByteString
getNinjaDeps baseDir opts@NinjaGraphOpts{..} =
  case ninjaGraphNinjaPath of
    Nothing -> BL.toStrict <$> generateNinjaDeps baseDir opts
    Just ninjaPath -> readNinjaDepsFile ninjaPath

scanNinjaDeps :: (Has Diagnostics sig m) => NinjaGraphOpts -> ByteString -> m [DepsTarget]
scanNinjaDeps NinjaGraphOpts{..} ninjaDepsContents =
  addInputsToNinjaDeps <$> ninjaDeps
  where
    ninjaDeps = parseNinjaDeps ninjaDepsContents

depsGraphEndpoint :: Url 'Https -> Url 'Https
depsGraphEndpoint baseurl = baseurl /: "depsGraph"

-- post the Ninja dependency graph data to the "Dependency graph" endpoint on Scotland Yard
-- POST /depsGraph
postDepsGraphResults :: (ToJSON a, MonadIO m, Has Diagnostics sig m) => NinjaGraphOpts -> a -> m ()
postDepsGraphResults NinjaGraphOpts{..} depsGraph = runHTTP $ do
  (baseUrl, baseOptions) <- parseUri depsGraphScotlandYardUrl
  _ <- req POST (depsGraphEndpoint baseUrl) (ReqBodyJson depsGraph) ignoreResponse (baseOptions <> header "Content-Type" "application/json")
  pure ()

readNinjaDepsFile :: (Has Trace sig m, Has ReadFS sig m, Has Diagnostics sig m, MonadIO m) => FilePath -> m ByteString
readNinjaDepsFile ninjaPath = do
  trace $ "reading ninja deps from " ++ ninjaPath
  path <- liftIO $ parseAbsFile ninjaPath
  readContentsBS path

generateNinjaDeps :: (Has Trace sig m, Has Diagnostics sig m, MonadIO m) => Path Abs Dir -> NinjaGraphOpts -> m BL.ByteString
generateNinjaDeps baseDir NinjaGraphOpts{..} = do
  trace $ "Generating ninja deps with this command: " ++ commandString
  (exitcode, stdout, stderr) <- PROC.readProcess (setWorkingDir (fromAbsDir baseDir) (PROC.shell commandString))
  case (exitcode, stdout, stderr) of
    (ExitSuccess, _, _) -> pure stdout
    (_, _, err) -> fatal (ErrorRunningNinja (T.pack (show err)))
  where
    commandString = case lunchTarget of
      Nothing -> "cd " ++ show baseDir ++ " && NINJA_ARGS=\"-t deps\" make"
      Just lunch ->  "cd " ++ show baseDir ++ " && source ./build/envsetup.sh && lunch " ++ T.unpack lunch ++ " && NINJA_ARGS=\"-t deps\" make"

addInputsToNinjaDeps :: [DepsTarget] -> [DepsTarget]
addInputsToNinjaDeps = map addInputToTarget

-- If there are any dependencies, then make inputs the first dependency
addInputToTarget :: DepsTarget -> DepsTarget
addInputToTarget target =
  correctedTarget target deps
  where
    deps = targetDependencies target

correctedTarget :: DepsTarget -> [DepsDependency] -> DepsTarget
correctedTarget target [] =
  target
correctedTarget target [singleDep] =
  target { targetDependencies = [], targetInputs = [singleDep] }
correctedTarget target (firstDep : secondDep : remainingDeps) =
  if targetPathWithoutExt /= firstDepWithoutExt && firstDepExt == ".txt" && secondDepWithoutExt == targetPathWithoutExt then
    target { targetDependencies = (firstDep : remainingDeps) , targetInputs = [secondDep]}
  else
    target { targetDependencies = (secondDep : remainingDeps), targetInputs = [firstDep]}
  where
    (targetPathWithoutExt, _) = FP.splitExtension $ FP.takeFileName $ T.unpack $ targetPath target
    (firstDepWithoutExt, firstDepExt) = FP.splitExtension $ FP.takeFileName $ T.unpack $ dependencyPath firstDep
    (secondDepWithoutExt, _) =  FP.splitExtension $ FP.takeFileName $ T.unpack $ dependencyPath secondDep

parseNinjaDeps :: (Has Diagnostics sig m) => ByteString -> m [DepsTarget]
parseNinjaDeps ninjaDepsLines =
  case finalState of
    Complete -> pure $ reverse reversedDependenciesResults
    Starting -> fatal NoNinjaDepsStartLineFound
    Parsing  -> fatal NoNinjaDepsEndLineFound
    Error    -> fatal NinjaDepsParseError
  where
    newLine = BS.head "\n" -- This is gross, but I couldn't get "BS.split '\n' ninjaDepsLines" to work
    nLines = BS.split newLine ninjaDepsLines
    (finalState, results) = foldl parseNinjaLine (Starting, []) nLines
    reversedDependenciesResults = map reverseDependencies results

-- The dependencies are reversed because we were consing them onto the
-- beginning of the array. Fix that here.
reverseDependencies :: DepsTarget -> DepsTarget
reverseDependencies target =
  target { targetDependencies = reverse deps }
  where
    deps = targetDependencies target

-- You can see a sample ninja deps file in test/App/VPSScan/testdata/small-ninja-deps
-- It starts with a preamble, which ends with a line that looks like
--
-- Starting ninja...
--
-- After the preamble there's the body.
-- The body itself consists of three types of lines:
-- blank lines, which are ignored
-- lines that start with a non-space character, which are target lines.
-- lines that start with spaces, which are dependency lines.
--
-- Target lines look like this:
--
-- out/target/product/coral/obj/JAVA_LIBRARIES/wifi-service_intermediates/dexpreopt.zip: #deps 2, deps mtime 1583991124 (VALID)
--
-- We just want the target, which is everything before the ": #deps"
--
-- Dependency lines are just four spaces followed by a path to a file. Like this:
--
--   device/google/coral/gpt-utils/gpt-utils.cpp
--
-- Again, we are only interested in the path, so we just strip off the leading spaces and return that.
--
-- The body ends with a line that looks like
-- [0;32m#### build completed successfully (20 seconds) ####[00m

parseNinjaLine :: ((NinjaParseState, [DepsTarget]) -> ByteString -> (NinjaParseState, [DepsTarget]))
parseNinjaLine (state, targets) line =
  case state of
    Starting ->
      if line == "Starting ninja..." then
        (Parsing, [])
      else
        (Starting, [])
    Parsing ->
      actuallyParseLine line targets
    Complete ->
      (Complete, targets)
    -- This should never happen
    _ -> (Error, targets)

actuallyParseLine :: ByteString -> [DepsTarget] -> (NinjaParseState, [DepsTarget])
-- ignore empty lines
actuallyParseLine "" targets =
  (Parsing, targets)

actuallyParseLine line []
-- error if you're trying to add a dependency and there are no targets yet
-- or if you reach the end of the file and no targets have been found
  | BS.isPrefixOf " " line || BS.isInfixOf "build completed successfully" line =
    (Error, [])
-- Add the first target
  | otherwise =
    (Parsing, [newDepsTarget])
  where
    newDepsTarget = targetFromLine line

actuallyParseLine line (currentDepsTarget : restOfDepsTargets)
-- The "build completed successfully" line signals that parsing is complete
  | BS.isInfixOf "build completed successfully" line =
    (Complete, currentDepsTarget : restOfDepsTargets)
-- Lines starting with a space add a new dep to the current target
  | BS.isPrefixOf " " line =
    (Parsing, updatedDepsTarget : restOfDepsTargets)
-- Lines starting with a non-blank char are new targets
  | otherwise =
    (Parsing, newDepsTarget : currentDepsTarget : restOfDepsTargets)
  where
    newDepsTarget = targetFromLine line
    updatedDepsTarget = addDepToDepsTarget currentDepsTarget line

targetFromLine :: ByteString -> DepsTarget
targetFromLine line =
  DepsTarget (decodeUtf8 tar) [] [] Nothing
  where
    (tar, _) = BS.breakSubstring ": #deps" line

addDepToDepsTarget :: DepsTarget -> ByteString -> DepsTarget
addDepToDepsTarget target line =
  target { targetDependencies = newDep : currentDeps}
  where
    currentDeps = targetDependencies target
    newDep = parseDepLine line

parseDepLine :: ByteString -> DepsDependency
parseDepLine line =
  DepsDependency (decodeUtf8 path) componentName hasDeps
  where
    path = stripLeadingSpace line
    componentName = Nothing -- TODO: get component name
    hasDeps = BS.isPrefixOf "out/" path

stripLeadingSpace :: ByteString -> ByteString
stripLeadingSpace = BS.dropWhile (\c -> c == BS.head " ")