{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze (
  analyzeMain,
  ScanDestination (..),
  UnpackArchives (..),
  JsonOutput (..),
  VSIAnalysisMode (..),
  IATAssertionMode (..),
  BinaryDiscoveryMode (..),
  ModeOptions (..),
) where

import App.Docs (userGuideUrl)
import App.Fossa.API.BuildLink (getFossaBuildUrl)
import App.Fossa.Analyze.Debug (collectDebugBundle, diagToDebug)
import App.Fossa.Analyze.GraphMangler (graphingToGraph)
import App.Fossa.Analyze.Project (ProjectResult (..), mkResult)
import App.Fossa.Analyze.Types
import App.Fossa.BinaryDeps (analyzeBinaryDeps)
import App.Fossa.FossaAPIV1 (UploadResponse (..), getProject, projectIsMonorepo, uploadAnalysis, uploadContributors)
import App.Fossa.ManualDeps (analyzeFossaDepsFile)
import App.Fossa.ProjectInference (inferProjectDefault, inferProjectFromVCS, mergeOverride, saveRevision)
import App.Fossa.VSI.IAT.AssertRevisionBinaries (assertRevisionBinaries)
import App.Fossa.VSIDeps (analyzeVSIDeps)
import App.Types (
  BaseDir (..),
  OverrideProject,
  ProjectMetadata,
  ProjectRevision (projectBranch, projectName, projectRevision),
 )
import App.Util (validateDir)
import Control.Carrier.AtomicCounter (AtomicCounter, runAtomicCounter)
import Control.Carrier.Debug (Debug, debugMetadata, debugScope, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Diagnostics.StickyContext
import Control.Carrier.Finally
import Control.Carrier.Output.IO
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool
import Control.Concurrent
import Control.Effect.Diagnostics (fatalText, fromMaybeText, recover, (<||>))
import Control.Effect.Exception (Lift)
import Control.Effect.Lift (sendIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Flag (Flag, fromFlag)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (isInfixOf, stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Discovery.Archive qualified as Archive
import Discovery.Filters
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec, runExecIO)
import Effect.Logger
import Effect.ReadFS (ReadFS, runReadFSIO)
import Fossa.API.Types (ApiOpts (..))
import Path (Abs, Dir, Path, fromAbsDir, toFilePath)
import Path.IO (makeRelative)
import Srclib.Converter qualified as Srclib
import Srclib.Types (Locator (locatorProject, locatorRevision), SourceUnit, parseLocator)
import Strategy.Bundler qualified as Bundler
import Strategy.Cargo qualified as Cargo
import Strategy.Carthage qualified as Carthage
import Strategy.Cocoapods qualified as Cocoapods
import Strategy.Composer qualified as Composer
import Strategy.Conda qualified as Conda
import Strategy.Glide qualified as Glide
import Strategy.Godep qualified as Godep
import Strategy.Gomodules qualified as Gomodules
import Strategy.Googlesource.RepoManifest qualified as RepoManifest
import Strategy.Gradle qualified as Gradle
import Strategy.Haskell.Cabal qualified as Cabal
import Strategy.Haskell.Stack qualified as Stack
import Strategy.Leiningen qualified as Leiningen
import Strategy.Maven qualified as Maven
import Strategy.Mix qualified as Mix
import Strategy.Npm qualified as Npm
import Strategy.NuGet.Nuspec qualified as Nuspec
import Strategy.NuGet.PackageReference qualified as PackageReference
import Strategy.NuGet.PackagesConfig qualified as PackagesConfig
import Strategy.NuGet.Paket qualified as Paket
import Strategy.NuGet.ProjectAssetsJson qualified as ProjectAssetsJson
import Strategy.NuGet.ProjectJson qualified as ProjectJson
import Strategy.Pub qualified as Pub
import Strategy.Python.Pipenv qualified as Pipenv
import Strategy.Python.Poetry qualified as Poetry
import Strategy.Python.Setuptools qualified as Setuptools
import Strategy.RPM qualified as RPM
import Strategy.Rebar3 qualified as Rebar3
import Strategy.Scala qualified as Scala
import Strategy.SwiftPM qualified as SwiftPM
import Strategy.Yarn qualified as Yarn
import Types (DiscoveredProject (..), FoundTargets)
import VCS.Git (fetchGitContributors)

data ScanDestination
  = -- | upload to fossa with provided api key and base url
    UploadScan ApiOpts ProjectMetadata
  | OutputStdout

-- | UnpackArchives bool flag
data UnpackArchives = UnpackArchives

data JsonOutput = JsonOutput

-- | Collect analysis modes into a single type for ease of use.
-- These modes are intended to be different options that alter how analysis is performed or what analysis steps are followed.
data ModeOptions = ModeOptions
  { modeVSIAnalysis :: VSIAnalysisMode
  , modeIATAssertion :: IATAssertionMode
  , modeBinaryDiscovery :: BinaryDiscoveryMode
  }

-- | "VSI analysis" modes
data VSIAnalysisMode
  = -- | enable the VSI analysis strategy
    VSIAnalysisEnabled
  | -- | disable the VSI analysis strategy
    VSIAnalysisDisabled

-- | "IAT Assertion" modes
data IATAssertionMode
  = -- | assertion enabled, reading binaries from this directory
    IATAssertionEnabled (Path Abs Dir)
  | -- | assertion not enabled
    IATAssertionDisabled

-- | "Binary Discovery" modes
data BinaryDiscoveryMode
  = -- | Binary discovery enabled
    BinaryDiscoveryEnabled
  | -- | Binary discovery disabled
    BinaryDiscoveryDisabled

analyzeMain :: FilePath -> Severity -> ScanDestination -> OverrideProject -> Flag UnpackArchives -> Flag JsonOutput -> ModeOptions -> AllFilters -> IO ()
analyzeMain workdir logSeverity destination project unpackArchives jsonOutput modeOptions filters =
  withDefaultLogger logSeverity
    . Diag.logWithExit_
    . runReadFSIO
    . runExecIO
    $ case logSeverity of
      -- In --debug mode, emit a debug bundle to "fossa.debug.json"
      SevDebug -> do
        basedir <- sendIO $ validateDir workdir
        (scope, res) <- collectDebugBundle . Diag.errorBoundaryIO $ doAnalyze basedir
        sendIO $ Aeson.encodeFile "fossa.debug.json" scope
        either Diag.rethrow pure res
      _ -> do
        basedir <- sendIO $ validateDir workdir
        ignoreDebug $ doAnalyze basedir
  where
    doAnalyze basedir = analyze basedir destination project unpackArchives jsonOutput modeOptions filters

runDependencyAnalysis ::
  ( AnalyzeProject proj
  , Aeson.ToJSON proj
  , Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has Logger sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has (Output ProjectResult) sig m
  , MonadIO m
  ) =>
  -- | Analysis base directory
  Path Abs Dir ->
  AllFilters ->
  DiscoveredProject proj ->
  m ()
runDependencyAnalysis basedir filters project = do
  case applyFiltersToProject basedir filters project of
    Nothing -> logInfo $ "Skipping " <> pretty (projectType project) <> " project at " <> viaShow (projectPath project) <> ": no filters matched"
    Just targets -> do
      logInfo $ "Analyzing " <> pretty (projectType project) <> " project at " <> pretty (toFilePath (projectPath project))
      graphResult <- Diag.runDiagnosticsIO . diagToDebug . stickyDiag . Diag.context "Project Analysis" $ do
        debugMetadata "DiscoveredProject" project
        analyzeProject targets (projectData project)
      Diag.withResult SevWarn graphResult (output . mkResult basedir project)

applyFiltersToProject :: Path Abs Dir -> AllFilters -> DiscoveredProject n -> Maybe FoundTargets
applyFiltersToProject basedir filters DiscoveredProject{..} =
  case makeRelative basedir projectPath of
    -- FIXME: this is required for --unpack-archives to continue to work.
    -- archives are not unpacked relative to the scan basedir, so "makeRelative"
    -- will always fail
    Nothing -> Just projectBuildTargets
    Just rel -> do
      applyFilters filters projectType rel projectBuildTargets

-- NOTE: When adding analyzers, make sure to also add them to
-- App.Fossa.ListTargets
runAnalyzers ::
  ( Has (Output ProjectResult) sig m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has TaskPool sig m
  , Has AtomicCounter sig m
  , Has (Lift IO) sig m
  , Has Debug sig m
  , MonadIO m
  ) =>
  Path Abs Dir ->
  AllFilters ->
  m ()
runAnalyzers basedir filters = do
  single Bundler.discover
  single Cabal.discover
  single Cargo.discover
  single Carthage.discover
  single Cocoapods.discover
  single Composer.discover
  single Conda.discover
  single Glide.discover
  single Godep.discover
  single Gomodules.discover
  single Gradle.discover
  single Leiningen.discover
  single Maven.discover
  single Mix.discover
  single Npm.discover
  single Nuspec.discover
  single PackageReference.discover
  single PackagesConfig.discover
  single Paket.discover
  single Pipenv.discover
  single Poetry.discover
  single ProjectAssetsJson.discover
  single ProjectJson.discover
  single Pub.discover
  single RPM.discover
  single Rebar3.discover
  single RepoManifest.discover
  single Scala.discover
  single Setuptools.discover
  single Stack.discover
  single SwiftPM.discover
  single Yarn.discover
  where
    single f = withDiscoveredProjects f basedir (runDependencyAnalysis basedir filters)

analyze ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Diag.Diagnostics sig m
  , Has Debug sig m
  , Has Exec sig m
  , Has ReadFS sig m
  , MonadIO m
  ) =>
  BaseDir ->
  ScanDestination ->
  OverrideProject ->
  Flag UnpackArchives ->
  Flag JsonOutput ->
  ModeOptions ->
  AllFilters ->
  m ()
analyze (BaseDir basedir) destination override unpackArchives jsonOutput ModeOptions{..} filters = Diag.context "fossa-analyze" $ do
  capabilities <- sendIO getNumCapabilities

  let apiOpts = case destination of
        OutputStdout -> Nothing
        UploadScan opts _ -> Just opts

  -- additional source units are built outside the standard strategy flow, because they either
  -- require additional information (eg API credentials), or they return additional information (eg user deps).
  manualSrcUnits <- Diag.context "fossa-deps" $ analyzeFossaDepsFile basedir apiOpts
  vsiResults <- Diag.context "analyze-vsi" $ analyzeVSI modeVSIAnalysis apiOpts basedir filters
  binarySearchResults <- Diag.context "discover-binaries" $ analyzeDiscoverBinaries modeBinaryDiscovery basedir filters
  let additionalSourceUnits :: [SourceUnit]
      additionalSourceUnits = catMaybes [manualSrcUnits, vsiResults, binarySearchResults]

  (projectResults, ()) <-
    Diag.context "discovery/analysis tasks"
      . runOutput @ProjectResult
      . runStickyLogger SevInfo
      . runFinally
      . withTaskPool capabilities updateProgress
      . runAtomicCounter
      $ do
        runAnalyzers basedir filters
        when (fromFlag UnpackArchives unpackArchives) $
          forkTask $ do
            res <- Diag.runDiagnosticsIO . diagToDebug . stickyDiag $ Archive.discover (`runAnalyzers` filters) basedir
            Diag.withResult SevError res (const (pure ()))

  let filteredProjects = filterProjects (BaseDir basedir) projectResults

  -- Need to check if vendored is empty as well, even if its a boolean that vendoredDeps exist
  case checkForEmptyUpload projectResults filteredProjects additionalSourceUnits of
    NoneDiscovered -> Diag.fatal ErrNoProjectsDiscovered
    FilteredAll count -> Diag.fatal (ErrFilteredAllProjects count projectResults)
    FoundSome sourceUnits -> case destination of
      OutputStdout -> do
        debugScope "DEBUG: Project inference" $ do
          inferred <- Diag.context "Inferring project name/revision" (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
          logDebug $ "Inferred revision: " <> viaShow inferred

          let revision = mergeOverride override inferred
          logDebug $ "Merged revision: " <> viaShow revision
        logStdout . decodeUtf8 . Aeson.encode $ buildResult additionalSourceUnits filteredProjects
      UploadScan opts metadata -> Diag.context "upload-results" $ do
        locator <- uploadSuccessfulAnalysis (BaseDir basedir) opts metadata jsonOutput override sourceUnits
        doAssertRevisionBinaries modeIATAssertion opts locator

analyzeVSI :: (MonadIO m, Has Diag.Diagnostics sig m, Has Exec sig m, Has (Lift IO) sig m, Has Logger sig m) => VSIAnalysisMode -> Maybe ApiOpts -> Path Abs Dir -> AllFilters -> m (Maybe SourceUnit)
analyzeVSI VSIAnalysisEnabled (Just apiOpts) dir filters = do
  logInfo "Running VSI analysis"
  results <- analyzeVSIDeps dir apiOpts filters
  pure $ Just results
analyzeVSI _ _ _ _ = pure Nothing

analyzeDiscoverBinaries :: (MonadIO m, Has Diag.Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m, Has ReadFS sig m) => BinaryDiscoveryMode -> Path Abs Dir -> AllFilters -> m (Maybe SourceUnit)
analyzeDiscoverBinaries BinaryDiscoveryEnabled dir filters = do
  logInfo "Discovering binary files as dependencies"
  analyzeBinaryDeps dir filters
analyzeDiscoverBinaries _ _ _ = pure Nothing

doAssertRevisionBinaries :: (Has Diag.Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has Logger sig m) => IATAssertionMode -> ApiOpts -> Locator -> m ()
doAssertRevisionBinaries (IATAssertionEnabled dir) apiOpts locator = assertRevisionBinaries dir apiOpts locator
doAssertRevisionBinaries _ _ _ = pure ()

dieOnMonorepoUpload :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => ApiOpts -> ProjectRevision -> m ()
dieOnMonorepoUpload apiOpts revision = do
  project <- recover $ getProject apiOpts revision
  if maybe False projectIsMonorepo project
    then fatalText "This project already exists as a monorepo project. Perhaps you meant to supply '--experimental-enable-monorepo', or meant to run 'fossa vps analyze' instead?"
    else pure ()

data AnalyzeError
  = ErrNoProjectsDiscovered
  | ErrFilteredAllProjects Int [ProjectResult]

instance Diag.ToDiagnostic AnalyzeError where
  renderDiagnostic :: AnalyzeError -> Doc ann
  renderDiagnostic ErrNoProjectsDiscovered =
    vsep
      [ "No analysis targets found in directory."
      , ""
      , "Make sure your project is supported. See the user guide for details:"
      , "    " <> pretty userGuideUrl
      , ""
      ]
  renderDiagnostic (ErrFilteredAllProjects count projectResults) =
    "Filtered out all "
      <> pretty count
      <> " projects due to directory name, and no manual deps were found."
      <> line
      <> line
      <> vsep (map renderExcludedProject projectResults)
    where
      renderExcludedProject :: ProjectResult -> Doc ann
      renderExcludedProject project = "Excluded by directory name: " <> pretty (toFilePath $ projectResultPath project)

uploadSuccessfulAnalysis ::
  ( Has Diag.Diagnostics sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has ReadFS sig m
  ) =>
  BaseDir ->
  ApiOpts ->
  ProjectMetadata ->
  Flag JsonOutput ->
  OverrideProject ->
  NE.NonEmpty SourceUnit ->
  m Locator
uploadSuccessfulAnalysis (BaseDir basedir) apiOpts metadata jsonOutput override units = Diag.context "Uploading analysis" $ do
  inferred <- Diag.context "Inferring project name/revision" (inferProjectFromVCS basedir <||> inferProjectDefault basedir)
  logDebug $ "Inferred revision: " <> viaShow inferred

  let revision = mergeOverride override inferred
  logDebug $ "Merged revision: " <> viaShow revision

  logInfo ""
  logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
  logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
  let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
  logInfo ("Using branch: `" <> pretty branchText <> "`")

  dieOnMonorepoUpload apiOpts revision
  saveRevision revision

  uploadResult <- uploadAnalysis apiOpts revision metadata units
  let locator = parseLocator $ uploadLocator uploadResult
  buildUrl <- getFossaBuildUrl revision apiOpts locator
  logInfo $
    vsep
      [ "============================================================"
      , ""
      , "    View FOSSA Report:"
      , "    " <> pretty buildUrl
      , ""
      , "============================================================"
      ]
  traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError uploadResult)
  -- Warn on contributor errors, never fail
  void . Diag.recover . runExecIO $ tryUploadContributors basedir apiOpts (uploadLocator uploadResult)

  if fromFlag JsonOutput jsonOutput
    then do
      summary <-
        Diag.context "Analysis ran successfully, but the server returned invalid metadata" $
          buildProjectSummary revision (uploadLocator uploadResult) buildUrl
      logStdout . decodeUtf8 $ Aeson.encode summary
    else pure ()

  pure locator

data CountedResult
  = NoneDiscovered
  | FilteredAll Int
  | FoundSome (NE.NonEmpty SourceUnit)

-- | Return some state of the projects found, since we can't upload empty result arrays.
-- Takes a list of all projects analyzed, and the list after filtering.  We assume
-- that the smaller list is the latter, and return that list.  Starting with user-defined deps,
-- we also include a check for an additional source unit from fossa-deps.yml.
checkForEmptyUpload :: [ProjectResult] -> [ProjectResult] -> [SourceUnit] -> CountedResult
checkForEmptyUpload xs ys additionalUnits = do
  if null additionalUnits
    then case (xlen, ylen) of
      -- We didn't discover, so we also didn't filter
      (0, 0) -> NoneDiscovered
      -- If either list is empty, we have nothing to upload
      (0, _) -> FilteredAll filterCount
      (_, 0) -> FilteredAll filterCount
      -- NE.fromList is a partial, but is safe since we confirm the length is > 0.
      _ -> FoundSome $ NE.fromList discoveredUnits
    else -- If we have a additional source units, then there's always something to upload.
      FoundSome $ NE.fromList (additionalUnits ++ discoveredUnits)
  where
    xlen = length xs
    ylen = length ys
    filterCount = abs $ xlen - ylen
    -- The smaller list is the post-filter list, since filtering cannot add projects
    filtered = if xlen > ylen then ys else xs
    discoveredUnits = map Srclib.toSourceUnit filtered

-- For each of the projects, we need to strip the root directory path from the prefix of the project path.
-- We don't want parent directories of the scan root affecting "production path" filtering -- e.g., if we're
-- running in a directory called "tmp", we still want results.
filterProjects :: BaseDir -> [ProjectResult] -> [ProjectResult]
filterProjects rootDir = filter (isProductionPath . dropPrefix rootPath . fromAbsDir . projectResultPath)
  where
    rootPath = fromAbsDir $ unBaseDir rootDir
    dropPrefix :: String -> String -> String
    dropPrefix prefix str = fromMaybe prefix (stripPrefix prefix str)

isProductionPath :: FilePath -> Bool
isProductionPath path =
  not $
    any
      (`isInfixOf` path)
      [ "doc/"
      , "docs/"
      , "test/"
      , "example/"
      , "examples/"
      , "vendor/"
      , "node_modules/"
      , ".srclib-cache/"
      , "spec/"
      , "Godeps/"
      , ".git/"
      , "bower_components/"
      , "third_party/"
      , "third-party/"
      , "Carthage/"
      , "Checkouts/"
      ]

tryUploadContributors ::
  ( Has Diag.Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs Dir ->
  ApiOpts ->
  -- | Locator
  Text ->
  m ()
tryUploadContributors baseDir apiOpts locator = do
  contributors <- fetchGitContributors baseDir
  uploadContributors apiOpts locator contributors

-- | Build project summary JSON to be output to stdout
buildProjectSummary :: Has Diag.Diagnostics sig m => ProjectRevision -> Text -> Text -> m Aeson.Value
buildProjectSummary project projectLocator projectUrl = do
  let locator = parseLocator projectLocator
  revision <- fromMaybeText "Server returned an invalid project revision" $ locatorRevision locator
  pure $
    Aeson.object
      [ "project" .= locatorProject locator
      , "revision" .= revision
      , "branch" .= projectBranch project
      , "url" .= projectUrl
      , "id" .= projectLocator
      ]

buildResult :: [SourceUnit] -> [ProjectResult] -> Aeson.Value
buildResult srcUnits projects =
  Aeson.object
    [ "projects" .= map buildProject projects
    , "sourceUnits" .= finalSourceUnits
    ]
  where
    finalSourceUnits = srcUnits ++ scannedUnits
    scannedUnits = map Srclib.toSourceUnit projects

buildProject :: ProjectResult -> Aeson.Value
buildProject project =
  Aeson.object
    [ "path" .= projectResultPath project
    , "type" .= projectResultType project
    , "graph" .= graphingToGraph (projectResultGraph project)
    ]

updateProgress :: Has StickyLogger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky'
    ( "[ "
        <> annotate (color Cyan) (pretty pQueued)
        <> " Waiting / "
        <> annotate (color Yellow) (pretty pRunning)
        <> " Running / "
        <> annotate (color Green) (pretty pCompleted)
        <> " Completed"
        <> " ]"
    )
