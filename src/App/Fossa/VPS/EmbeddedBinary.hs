{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.VPS.EmbeddedBinary 
  ( BinaryPaths(..)
  , extractEmbeddedBinaries
  , cleanupExtractedBinaries
  , withEmbeddedBinaries
  ) where

import Prelude hiding (writeFile)
import Control.Effect.Exception (bracket)
import Control.Effect.Lift ( Has, Lift )
import Control.Monad.IO.Class
import Data.ByteString (writeFile, ByteString)
import Path
import Path.IO
import Data.FileEmbed.Extra

data BinaryPaths = BinaryPaths
  { binaryPathContainer :: Path Abs Dir
  , wigginsBinaryPath :: Path Abs File
  }

withEmbeddedBinaries :: (Has (Lift IO) sig m, MonadIO m) => (BinaryPaths -> m c) -> m c
withEmbeddedBinaries = bracket extractEmbeddedBinaries cleanupExtractedBinaries

cleanupExtractedBinaries :: (MonadIO m) => BinaryPaths -> m ()
cleanupExtractedBinaries BinaryPaths{..} = do
  removeDirRecur binaryPathContainer
  pure ()

extractEmbeddedBinaries :: (MonadIO m) => m BinaryPaths
extractEmbeddedBinaries = do
  container <- extractDir

  -- Determine paths to which we should write the binaries
  wigginsBinaryPath <- extractedPath $(mkRelFile "wiggins")

  -- Write the binaries
  liftIO $ writeExecutable wigginsBinaryPath embeddedBinaryWiggins

  -- Return the paths
  pure (BinaryPaths container wigginsBinaryPath)

writeExecutable :: Path Abs File -> ByteString -> IO ()
writeExecutable path content = do
  ensureDir $ parent path
  writeFile (fromAbsFile path) content
  makeExecutable path

extractedPath :: MonadIO m => Path Rel File -> m (Path Abs File)
extractedPath name = do
  dir <- extractDir
  pure (dir </> name)

extractDir :: MonadIO m => m (Path Abs Dir)
extractDir = do
  wd <- liftIO getTempDir
  pure (wd </> $(mkRelDir "vpscli-vendor"))

makeExecutable :: Path Abs File -> IO ()
makeExecutable path = do
  p <- getPermissions path
  setPermissions path (p {executable = True})
  
-- The intent with these embedded binaries is that the build system will replace the files with built binaries of the appropriate architecture.
-- The versions vendored into the repository are suitable for running on MacOS.
-- The below functions are expectd to warn since the vendor directory is typically populated in CI.
-- If you wish to build `vpscli` for your local system, populate these binaries via `vendor_download.sh`.
embeddedBinaryWiggins :: ByteString
embeddedBinaryWiggins = $(embedFileIfExists "vendor/wiggins")





