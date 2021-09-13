{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.BinaryDeps.Archive (withArchive, extractZip) where

import Codec.Archive.Zip qualified as Zip
import Control.Algebra (Has)
import Control.Effect.Exception (bracket)
import Control.Effect.Lift (Lift, sendIO)
import Path (Abs, Dir, File, Path, fromAbsDir, fromAbsFile, mkRelDir, (</>))
import Path.IO (getTempDir, removeDirRecur)

-- | Use the provided extraction function to extract the zip, then run the callback.
-- When the callback is complete, clean up the temporary directory holding the archive contents.
withArchive :: Has (Lift IO) sig m => (Path Abs File -> Path Abs Dir -> m ()) -> Path Abs File -> (Path Abs Dir -> m c) -> m c
withArchive extract archive = bracket (extractArchive extract archive) cleanupDir

cleanupDir :: Has (Lift IO) sig m => Path Abs Dir -> m ()
cleanupDir dir = sendIO $ removeDirRecur dir

mkExtractDir :: Has (Lift IO) sig m => m (Path Abs Dir)
mkExtractDir = do
  wd <- sendIO getTempDir
  pure (wd </> $(mkRelDir "fossa-extract"))

extractArchive :: Has (Lift IO) sig m => (Path Abs File -> Path Abs Dir -> m ()) -> Path Abs File -> m (Path Abs Dir)
extractArchive extract archive = do
  container <- mkExtractDir
  extract archive container
  pure container

-- | Extract a zip-formatted archive.
extractZip :: Has (Lift IO) sig m => Path Abs File -> Path Abs Dir -> m ()
extractZip archive to =
  sendIO $ Zip.withArchive (fromAbsFile archive) (Zip.unpackInto (fromAbsDir to))
