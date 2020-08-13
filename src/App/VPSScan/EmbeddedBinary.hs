{-# LANGUAGE TemplateHaskell #-}

module App.VPSScan.EmbeddedBinary 
  ( withUnpackedSherlockCli
  , withUnpackedIPRClis
  , IPRBinaryPaths(..)
  ) where

import Prelude hiding (writeFile)
import Control.Effect.Exception
import Control.Monad.IO.Class
import Data.ByteString (writeFile, ByteString)
import Path.IO (removeDirRecur, createTempDir, getTempDir)
import Path.Posix hiding ((</>))
import System.FilePath ((</>))
import System.Directory (Permissions(executable), setPermissions, getPermissions)
import Data.FileEmbed.Extra

withUnpackedSherlockCli :: (Has (Lift IO) sig m, MonadIO m) => (FilePath -> m a) -> m a
withUnpackedSherlockCli act =
  bracket (liftIO getTempDir >>= \tmp -> createTempDir tmp "fossa-vpscli-vendor-sherlock")
          (liftIO . removeDirRecur)
          go
  where
    go tmpDir = do
      let binaryPath = fromAbsDir tmpDir </> "sherlock-cli"
      liftIO (writeFile binaryPath embeddedBinarySherlockCli)
      liftIO (makeExecutable binaryPath)
      act binaryPath

data IPRBinaryPaths = IPRBinaryPaths
  { ramjetBinaryPath :: FilePath
  , nomosBinaryPath :: FilePath
  , pathfinderBinaryPath :: FilePath
  }

withUnpackedIPRClis :: (Has (Lift IO) sig m, MonadIO m) => (IPRBinaryPaths -> m a) -> m a
withUnpackedIPRClis act = 
  bracket (liftIO getTempDir >>= \tmp -> createTempDir tmp "fossa-vpscli-vendor-ipr")
          (liftIO . removeDirRecur)
          go
  where
    go tmpDir = do
      let root = fromAbsDir tmpDir
      let paths = IPRBinaryPaths (root </> "ramjet-cli-ipr") (root </> "nomossa") (root </> "pathfinder")
      liftIO (writeFile (ramjetBinaryPath paths) embeddedBinaryRamjetCli)
      liftIO (writeFile (nomosBinaryPath paths) embeddedBinaryNomossa)
      liftIO (writeFile (pathfinderBinaryPath paths) embeddedBinaryPathfinder)
      liftIO (makeExecutable (ramjetBinaryPath paths))
      liftIO (makeExecutable (nomosBinaryPath paths))
      liftIO (makeExecutable (pathfinderBinaryPath paths))
      act paths

makeExecutable :: FilePath -> IO ()
makeExecutable f = do
  p <- getPermissions f
  setPermissions f (p {executable = True})
  
-- The intent with these embedded binaries is that the build system will replace the files with built binaries of the appropriate architecture.
-- The versions vendored into the repository are suitable for running on MacOS.
-- The below functions are expectd to warn since the vendor directory is typically populated in CI.
-- If you wish to build `vpscli` for your local system, populate these binaries via `vendor_download.sh`.
embeddedBinarySherlockCli :: ByteString
embeddedBinarySherlockCli = $(embedFileIfExists "vendor/sherlock-cli")

embeddedBinaryRamjetCli :: ByteString
embeddedBinaryRamjetCli = $(embedFileIfExists "vendor/ramjet-cli-ipr")

embeddedBinaryPathfinder :: ByteString
embeddedBinaryPathfinder = $(embedFileIfExists "vendor/pathfinder")

embeddedBinaryNomossa :: ByteString
embeddedBinaryNomossa = $(embedFileIfExists "vendor/nomossa")
