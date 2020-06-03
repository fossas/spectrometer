{-# language QuasiQuotes #-}

module App.Fossa.ProjectInference
  ( inferProject
  , InferredProject(..)
  ) where

import Prologue

import Control.Algebra
import Control.Effect.Diagnostics
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Path.IO (getTempDir)
import qualified System.FilePath.Posix as FP
import Text.GitConfig.Parser (Section(..), parseConfig)
import Text.Megaparsec (errorBundlePretty)

import Effect.Exec
import Effect.Logger
import Effect.ReadFS

inferProject :: (Has Logger sig m, MonadIO m) => Path Abs Dir -> m InferredProject
inferProject current = do
  inferred <- runDiagnostics $ runReadFSIO $ runExecIO $ (inferGit current <||> inferSVN current)

  case inferred of
    Right project -> pure project
    Left err -> do
      logWarn "Project inference: couldn't find VCS root. Defaulting to directory name."
      logDebug (pretty (diagnosticBundlePretty err))
      inferDefault current

svnCommand :: Command
svnCommand = Command
  { cmdName = "svn"
  , cmdArgs = ["info"]
  , cmdAllowErr = Never
  }

inferSVN :: (Has Exec sig m, Has Diagnostics sig m) => Path b Dir -> m InferredProject
inferSVN dir = do
  output <- execThrow dir svnCommand
  let props = toProps output
  case (,) <$> lookup "Repository Root" props <*> lookup "Revision" props of
    Just (name, rev) -> pure (InferredProject name rev)
    Nothing -> fatal (CommandParseError svnCommand "Invalid output (missing Repository Root or Revision)")

  where
  toProps :: BL.ByteString -> [(Text, Text)]
  toProps bs = mapMaybe toProp (T.lines (TL.toStrict (decodeUtf8 bs)))

  toProp :: Text -> Maybe (Text, Text)
  toProp propLine =
    case T.splitOn ": " propLine of
      [key, val] -> Just (key, val)
      _ -> Nothing

-- | Infer a default project name from the directory, and a default
-- revision from the current time. Writes `.fossa.revision` to the system
-- temp directory for use by `fossa test`
inferDefault :: MonadIO m => Path b Dir -> m InferredProject
inferDefault dir = liftIO $ do
  let name = FP.dropTrailingPathSeparator (fromRelDir (dirname dir))
  time <- floor <$> getPOSIXTime :: IO Int

  tmp <- getTempDir
  writeFile (fromAbsDir tmp FP.</> ".fossa.revision") (show time)

  pure (InferredProject (T.pack name) (T.pack (show time)))


findGitDir :: Has ReadFS sig m => Path Abs Dir -> m (Maybe (Path Abs Dir))
findGitDir dir = do
  let relGit = [reldir|.git|]

  exists <- doesDirExist (dir </> relGit)
  if exists
    then pure (Just (dir </> relGit))
    else do
      let parentDir = parent dir
      if parentDir /= dir
        then findGitDir parentDir
        else pure Nothing

inferGit ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) => Path Abs Dir -> m InferredProject
inferGit dir = do
  foundGitDir <- findGitDir dir

  case foundGitDir of
    Nothing -> fatal MissingGitDir
    Just gitDir -> do
      name     <- parseGitProjectName gitDir
      revision <- parseGitProjectRevision gitDir
      pure (InferredProject name revision)

parseGitProjectName ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  )
  => Path Abs Dir -> m Text
parseGitProjectName dir = do
  let relConfig = [relfile|config|]

  exists <- doesFileExist (dir </> relConfig)

  unless exists (fatal MissingGitConfig)

  contents <- readContentsText (dir </> relConfig)

  case parseConfig contents of
    Left err -> fatal (GitConfigParse (T.pack (errorBundlePretty err)))

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
  )
  => Path Abs Dir -> m Text
parseGitProjectRevision dir = do
  let relHead = [relfile|HEAD|]

  headExists <- doesFileExist (dir </> relHead)

  unless headExists (fatal MissingGitHead)

  contents <- removeNewlines . T.drop 5 <$> readContentsText (dir </> relHead)

  case parseRelFile (T.unpack contents) of
    Just path -> do
      branchExists <- doesFileExist (dir </> path)

      unless branchExists (fatal (MissingBranch contents))

      removeNewlines <$> readContentsText (dir </> path)

    Nothing -> fatal (InvalidBranchName contents)

removeNewlines :: Text -> Text
removeNewlines = T.replace "\r" "" . T.replace "\n" ""

data InferenceError =
    InvalidRemote
  | GitConfigParse Text
  | MissingGitConfig
  | MissingGitHead
  | InvalidBranchName Text
  | MissingBranch Text
  | MissingGitDir
  deriving (Eq, Ord, Show, Generic, Typeable)

-- FIXME
instance ToDiagnostic InferenceError where

data InferredProject = InferredProject
  { inferredName     :: Text
  , inferredRevision :: Text
  } deriving (Eq, Ord, Show, Generic)
