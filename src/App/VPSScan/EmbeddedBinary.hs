{-# LANGUAGE TemplateHaskell #-}

module App.VPSScan.EmbeddedBinary 
  ( extractEmbeddedBinary
  ) where

import Prelude ((++), ($), pure, error, FilePath)
import Data.Text
import Control.Monad.IO.Class
import Data.ByteString (writeFile, ByteString)
import Data.FileEmbed (embedFile)
import System.Directory
import System.FilePath ((</>))

extractEmbeddedBinary :: (MonadIO m) => Text -> m FilePath
extractEmbeddedBinary "sherlock-cli" = do
  path <- absBinaryPath "sherlock-cli"
  liftIO $ writeFile path embeddedBinarySherlockCli
  pure path
extractEmbeddedBinary "ramjet-cli-ipr" = do
  path <- absBinaryPath "ramjet-cli-ipr"
  liftIO $ writeFile path embeddedBinaryRamjetCli
  pure path
extractEmbeddedBinary "pathfinder" = do
  path <- absBinaryPath "pathfinder"
  liftIO $ writeFile path embeddedBinaryPathfinder
  pure path
extractEmbeddedBinary "nomossa" = do
  path <- absBinaryPath "nomossa"
  liftIO $ writeFile path embeddedBinaryNomossa
  pure path
extractEmbeddedBinary name = error $ "unbundled binary: " ++ unpack name

absBinaryPath :: (MonadIO m) => Text -> m FilePath
absBinaryPath name = do
  wd <- liftIO getCurrentDirectory
  pure (wd </> unpack name)

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
