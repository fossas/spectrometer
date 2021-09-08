{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.ProjectInference (
  inferProjectFromVCS,
  inferProjectCached,
  inferProjectDefault,
  saveRevision,
  mergeOverride,
  readCachedRevision,
  InferredProject (..),

  -- * Exported for testing.
  parseGitProjectRevision,
) where

import App.Types (OverrideProject (..), ProjectRevision (..))
import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (..),
  fatal,
  (<||>),
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (find)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Effect.Exec (
  AllowErr (..),
  Command (..),
  Exec,
  ExecErr (..),
  execThrow,
  runExecIO,
 )
import Effect.Logger (pretty)
import Effect.ReadFS (
  ReadFS,
  doesDirExist,
  doesFileExist,
  readContentsText,
  runReadFSIO,
 )
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  dirname,
  fromAbsFile,
  fromRelDir,
  mkRelFile,
  parent,
  parseRelFile,
  reldir,
  relfile,
  (</>),
 )
import Path.IO (getTempDir)
import System.FilePath.Posix qualified as FP
import Text.GitConfig.Parser (Section (..), parseConfig)
import Text.Megaparsec (errorBundlePretty)
import Debug.Trace (trace)

revisionFileName :: Path Rel File
revisionFileName = $(mkRelFile ".fossa.revision")

mergeOverride :: OverrideProject -> InferredProject -> ProjectRevision
mergeOverride OverrideProject{..} InferredProject{..} = ProjectRevision name revision branch
  where
    name = fromMaybe inferredName overrideName
    revision = fromMaybe inferredRevision overrideRevision
    branch = overrideBranch <|> inferredBranch

-- TODO: pass ReadFS and Exec constraints upward
inferProjectFromVCS :: (Has Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> m InferredProject
inferProjectFromVCS current = runReadFSIO $ runExecIO (inferGit current <||> inferSVN current)

-- | Similar to 'inferProjectDefault', but uses a saved revision
inferProjectCached :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path b Dir -> m InferredProject
inferProjectCached dir = do
  project <- inferProjectDefault dir
  rev <- readCachedRevision
  pure project{inferredRevision = rev}

-- | Infer a default project name from the directory, and a default
-- revision from the current time. Writes `.fossa.revision` to the system
-- temp directory for use by `fossa test`
inferProjectDefault :: Has (Lift IO) sig m => Path b Dir -> m InferredProject
inferProjectDefault dir = sendIO $ do
  let name = FP.dropTrailingPathSeparator (fromRelDir (dirname dir))
  time <- floor <$> getPOSIXTime :: IO Int

  pure (InferredProject (toText name) (toText (show time)) Nothing)

svnCommand :: Command
svnCommand =
  Command
    { cmdName = "svn"
    , cmdArgs = ["info"]
    , cmdAllowErr = Never
    }

inferSVN :: (Has Exec sig m, Has Diagnostics sig m) => Path b Dir -> m InferredProject
inferSVN dir = do
  output <- execThrow dir svnCommand
  let props = toProps output

  let maybeProject = do
        root <- lookup "Repository Root" props
        revision <- lookup "Revision" props
        url <- lookup "URL" props
        relUrl <- lookup "Relative URL" props

        let rootRelativeToUrl = dropPrefix url root

        -- we need to trim off: the caret, the root (as relative to the url), and one of "/branches/" or "/"
        let trimmedRelative =
              dropPrefix "branches/"
                . dropPrefix "/"
                . dropPrefix rootRelativeToUrl
                . dropPrefix "^"
                $ relUrl

        pure . InferredProject root revision $ if Text.null trimmedRelative then Nothing else Just trimmedRelative

  case maybeProject of
    Nothing -> fatal (CommandParseError svnCommand "Invalid output (missing Repository Root or Revision)")
    Just project -> pure project
  where
    toProps :: BL.ByteString -> [(Text, Text)]
    toProps bs = mapMaybe toProp (Text.lines (decodeUtf8 bs))
    toProp :: Text -> Maybe (Text, Text)
    toProp propLine =
      case Text.splitOn ": " propLine of
        [key, val] -> Just (key, val)
        _ -> Nothing

saveRevision :: Has (Lift IO) sig m => ProjectRevision -> m ()
saveRevision project = do
  tmp <- sendIO getTempDir
  sendIO $ TIO.writeFile (fromAbsFile $ tmp </> revisionFileName) (projectRevision project)

readCachedRevision :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => m Text
readCachedRevision = do
  tmp <- sendIO getTempDir
  readContentsText $ tmp </> revisionFileName

-- like Text.stripPrefix, but with a non-Maybe result (defaults to the original text)
dropPrefix :: Text -> Text -> Text
dropPrefix pre txt = fromMaybe txt (Text.stripPrefix pre txt)

findGitDir :: Has ReadFS sig m => Path Abs Dir -> m (Maybe (Path Abs Dir))
findGitDir dir = do
  let relGit = [reldir|.git|]

  let x = dir </> relGit
  exists <- doesDirExist $ trace ("testing dir: " <> show x) x
  if trace ("findGitDir: " <> show exists) exists
    then pure (Just (dir </> relGit))
    else do
      let parentDir = parent dir
      if parentDir /= dir
        then findGitDir parentDir
        else pure Nothing

inferGit ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m InferredProject
inferGit dir = do
  foundGitDir <- findGitDir dir

  case trace ("inferGit: " <> show foundGitDir) foundGitDir of
    Nothing -> fatal MissingGitDir
    Just gitDir -> do
      name <- parseGitProjectName gitDir
      (branch, revision) <- parseGitProjectRevision gitDir
      pure (InferredProject name revision branch)

parseGitProjectName ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m Text
parseGitProjectName dir = do
  let relConfig = [relfile|config|]

  exists <- doesFileExist (dir </> relConfig)

  unless exists (fatal MissingGitConfig)

  contents <- readContentsText (dir </> relConfig)

  case parseConfig contents of
    Left err -> fatal (GitConfigParse (toText (errorBundlePretty err)))
    Right config -> do
      let maybeSection = find isOrigin config
      case maybeSection of
        Nothing -> fatal InvalidRemote
        Just (Section _ properties) ->
          case HM.lookup "url" properties of
            Just url -> pure url
            Nothing -> fatal InvalidRemote
  where
    isOrigin :: Section -> Bool
    isOrigin (Section ["remote", "origin"] _) = True
    isOrigin _ = False

parseGitProjectRevision ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Maybe Text, Text) -- branch, revision
parseGitProjectRevision dir = do
  let relHead = [relfile|HEAD|]

  headExists <- doesFileExist (dir </> relHead)

  _ <- trace ("headExists: " <> show headExists) pure ()

  unless headExists (fatal MissingGitHead)

  headText <- readContentsText (dir </> relHead)

  _ <- trace ("headText: " <> show headText) pure ()

  if "ref: " `Text.isPrefixOf` headText
    then do
      let rawPath = removeNewlines . dropPrefix "ref: " $ headText

      case parseRelFile (toString rawPath) of
        Nothing -> fatal (InvalidBranchName rawPath)
        Just path -> do
          branchExists <- doesFileExist (dir </> path)

          unless branchExists (fatal (MissingBranch rawPath))

          revision <- removeNewlines <$> readContentsText (dir </> path)
          let branch = dropPrefix "refs/heads/" rawPath
          pure (Just branch, revision)
    else pure (Nothing, Text.strip headText)

removeNewlines :: Text -> Text
removeNewlines = Text.replace "\r" "" . Text.replace "\n" ""

data InferenceError
  = InvalidRemote
  | GitConfigParse Text
  | MissingGitConfig
  | MissingGitHead
  | InvalidBranchName Text
  | MissingBranch Text
  | MissingGitDir
  deriving (Eq, Ord, Show)

instance ToDiagnostic InferenceError where
  renderDiagnostic = \case
    InvalidRemote -> "Missing 'origin' git remote"
    GitConfigParse err -> "An error occurred when parsing the git config: " <> pretty err
    MissingGitConfig -> "Missing .git/config file"
    MissingGitHead -> "Missing .git/HEAD file"
    InvalidBranchName branch -> "Invalid branch name: " <> pretty branch
    MissingBranch branch -> "Missing ref file for current branch: " <> pretty branch
    MissingGitDir -> "Could not find .git directory in the current or any parent directory"

data InferredProject = InferredProject
  { inferredName :: Text
  , inferredRevision :: Text
  , inferredBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show)
