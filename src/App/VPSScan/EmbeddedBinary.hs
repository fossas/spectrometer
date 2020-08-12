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
import Data.FileEmbed (embedFile)
import Path.IO (removeDirRecur, createTempDir, getTempDir)
import Path.Posix hiding ((</>))
import System.FilePath ((</>))
import System.Directory (Permissions(executable))
import System.Directory (setPermissions)
import System.Directory (getPermissions)

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
embeddedBinarySherlockCli :: ByteString
embeddedBinarySherlockCli = $(embedFile "vendor/sherlock-cli")

embeddedBinaryRamjetCli :: ByteString
embeddedBinaryRamjetCli = $(embedFile "vendor/ramjet-cli-ipr")

embeddedBinaryPathfinder :: ByteString
embeddedBinaryPathfinder = $(embedFile "vendor/pathfinder")

embeddedBinaryNomossa :: ByteString
embeddedBinaryNomossa = $(embedFile "vendor/nomossa")
