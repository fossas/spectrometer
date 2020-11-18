{-# LANGUAGE TemplateHaskell #-}

module App.Version.TH
  ( getCurrentTag,
  )
where

import Control.Carrier.Diagnostics (resultValue, runDiagnostics)
import Control.Effect.Diagnostics
  ( Diagnostics,
    fromEitherShow,
  )
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Versions
import Effect.Exec
import GitHash
import Instances.TH.Lift ()
import Language.Haskell.TH
import Path
import Language.Haskell.TH.Syntax (addDependentFile)
import Path.IO (getCurrentDir)


gitTagPointCommand :: Text -> Command
gitTagPointCommand commit =
  Command
    { cmdName = "git",
      cmdArgs = ["tag", "--points-at", commit],
      cmdAllowErr = Always
    }

gitLogFile :: Path Rel File
gitLogFile = $(mkRelFile ".git/logs/HEAD")

getCurrentTag :: TExpQ (Maybe Text)
getCurrentTag = do
  let commitHash = giHash $$(tGitInfoCwd)
  result <- runIO . runDiagnostics . runExecIO . getTags $ T.pack commitHash

  {- addDependentFile will cause this function to recompile when .git/logs/HEAD
      changes. This is required for accuracy during dev since this value is not
      always needed in the final binary.

      NOTE that there is poentially a bug with this function, which causes it to
      do nothing.  https://github.com/haskell/cabal/issues/4746
  -}
  logFile <- runIO $ (</> gitLogFile) <$> getCurrentDir
  addDependentFile $ toFilePath logFile

  case result of
    Left err -> reportWarning (show err) >> [||Nothing||]
    Right tags -> filterTags $ resultValue tags

getTags :: (Has Exec sig m, Has Diagnostics sig m) => Text -> m [Text]
getTags hash = do
  -- FIXME: boy it would be nice to not shell out during compilation.  So much that could go wrong.
  result <- exec $(mkRelDir ".") $ gitTagPointCommand hash
  bsl <- fromEitherShow result

  pure . map T.strip . T.lines . TE.decodeUtf8 $ BSL.toStrict bsl

{- We'd like to use this time to make sure we have tags when we build release
    versions.  However, 2 things make that difficult at the time of writing:

    * We don't know if we're in a GH PR or a Release build.
    * We only build releases by pushing tags, so we can't even trigger the error case

    For these reasons, we're ignoring the case where there is no tag.  Future enhancements
    should apply to the [] case of the filterTags function below.  Theoretically, we COULD
    do some IO to determine something about github, and execute using `runIO`.
-}
filterTags :: [Text] -> TExpQ (Maybe Text)
filterTags [] = [||Nothing||]
filterTags [x] = validateSingleTag x
filterTags xs = reportWarning (T.unpack multiTagMesg) >> [||Nothing||]
  where
    multiTagMesg = header <> T.intercalate ", " xs
    header = "Multiple tags defined at current commit: "

validateSingleTag :: Text -> TExpQ (Maybe Text)
validateSingleTag tag = do
  let normalized = fromMaybe tag $ T.stripPrefix "v" tag

  case semver normalized of
    Left err -> reportWarning (errorBundlePretty err) >> [||Nothing||]
    Right _ -> [|| Just normalized ||]