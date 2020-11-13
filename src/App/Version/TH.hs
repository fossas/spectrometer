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

gitIndexFile :: Path Rel File
gitIndexFile = $(mkRelFile ".git/index")

getCurrentTag :: TExpQ (Maybe Text)
getCurrentTag = do
  let commitHash = giHash $$(tGitInfo ".")
  -- FIXME: boy it would be nice to not shell out during compilation.  So much that could go wrong.
  indexFile <- runIO $ (</> gitIndexFile) <$> getCurrentDir
  addDependentFile $ toFilePath indexFile
  result <- runIO . runDiagnostics . runExecIO . getTags $ T.pack commitHash

  case result of
    Left err -> reportWarning (show err) >> [||Nothing||]
    Right tags -> filterTags $ resultValue tags

getTags :: (Has Exec sig m, Has Diagnostics sig m) => Text -> m [Text]
getTags hash = do
  result <- exec $(mkRelDir ".") $ gitTagPointCommand hash
  bsl <- fromEitherShow result

  pure . map T.strip . T.lines . TE.decodeUtf8 $ BSL.toStrict bsl

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

  case versioning tag of
    Left err -> reportWarning (errorBundlePretty err) >> [||Nothing||]
    Right _ -> [||Just normalized||]