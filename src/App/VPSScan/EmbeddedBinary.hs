{-# LANGUAGE TemplateHaskell #-}

module App.VPSScan.EmbeddedBinary 
  ( extractEmbeddedBinary
  ) where

import Prelude hiding (writeFile)
import Data.Text
import Control.Monad.IO.Class
import Data.ByteString (writeFile, ByteString)
import Data.FileEmbed (embedFile)
import System.Directory
import System.FilePath (takeDirectory, (</>))
import Language.Haskell.TH.Syntax

extractEmbeddedBinary :: (MonadIO m) => Text -> m FilePath
extractEmbeddedBinary "sherlock-cli" = do
  path <- absBinaryPath "sherlock-cli"
  liftIO $ writeExecutable path embeddedBinarySherlockCli
  pure path
extractEmbeddedBinary "ramjet-cli-ipr" = do
  path <- absBinaryPath "ramjet-cli-ipr"
  liftIO $ writeExecutable path embeddedBinaryRamjetCli
  pure path
extractEmbeddedBinary "pathfinder" = do
  path <- absBinaryPath "pathfinder"
  liftIO $ writeExecutable path embeddedBinaryPathfinder
  pure path
extractEmbeddedBinary "nomossa" = do
  path <- absBinaryPath "nomossa"
  liftIO $ writeExecutable path embeddedBinaryNomossa
  pure path
extractEmbeddedBinary name = error $ "unbundled binary: " ++ unpack name

writeExecutable :: FilePath -> ByteString -> IO ()
writeExecutable path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content
  makeExecutable path

makeExecutable :: FilePath -> IO ()
makeExecutable f = do
  p <- getPermissions f
  setPermissions f (p {executable = True})

absBinaryPath :: (MonadIO m) => Text -> m FilePath
absBinaryPath name = do
  wd <- liftIO getCurrentDirectory
  pure (wd </> ".vendor" </> unpack name)

-- The intent with these embedded binaries is that the build system will replace the files with built binaries of the appropriate architecture.
-- The versions vendored into the repository are suitable for running on MacOS.
embeddedBinarySherlockCli :: ByteString
embeddedBinarySherlockCli = $(addDependentFile "vendor/sherlock-cli" >> embedFile "vendor/sherlock-cli")

embeddedBinaryRamjetCli :: ByteString
embeddedBinaryRamjetCli = $(addDependentFile "vendor/ramjet-cli-ipr" >> embedFile "vendor/ramjet-cli-ipr")

embeddedBinaryPathfinder :: ByteString
embeddedBinaryPathfinder = $(addDependentFile "vendor/pathfinder" >> embedFile "vendor/pathfinder")

embeddedBinaryNomossa :: ByteString
embeddedBinaryNomossa = $(addDependentFile "vendor/nomossa" >> embedFile "vendor/nomossa")
