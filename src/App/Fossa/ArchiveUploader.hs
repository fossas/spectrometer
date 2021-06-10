{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module App.Fossa.ArchiveUploader
  ( archiveUploadSourceUnit,
    archivesNoUploadSourceUnit, 
  )
where

import Debug.Trace

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import Path hiding ((</>))
import Data.Text (Text)
import App.Fossa.YamlDeps
import qualified Data.Text as T
import Crypto.Hash
import qualified App.Fossa.FossaAPIV1 as Fossa
import Fossa.API.Types
import qualified Control.Carrier.Diagnostics as Diag
import System.FilePath.Posix
import Text.URI (mkURI)
import System.IO.Temp ( withSystemTempDirectory )
import Srclib.Types (Locator (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (SourceUnitDependency))
import Data.Maybe (fromMaybe)

md5 :: BS.ByteString  -> Digest MD5
md5 = hashlazy

-- testDir :: Path Abs Dir
-- testDir = $(mkAbsDir "/Users/zachlavallee/Programming/Fossa-Tools/spectrometer/")

uploadArchives :: ApiOpts -> [VendoredDependency] -> FilePath -> FilePath -> IO [Archive]
uploadArchives apiOpts deps arcDir tmpDir = traverse (compressAndUpload apiOpts arcDir tmpDir) deps

compressAndUpload :: ApiOpts -> FilePath -> FilePath -> VendoredDependency -> IO Archive
compressAndUpload apiOpts arcDir tmpDir dependency = do
      compressedFile <- compressFile tmpDir arcDir (T.unpack $ vendoredPath dependency)


      depVersion <- case vendoredVersion dependency of
                      Nothing -> do
                                   fileHash <- hashFile compressedFile
                                   pure $ case fileHash of
                                         -- Need to exit with an error here
                                         -- Nothing -> traceM "No file hash able to be generated"
                                         Nothing -> T.pack "asd"
                                         Just md5Hash -> T.pack $ show md5Hash
                      Just version -> pure version

      response <- Diag.runDiagnostics $ Fossa.getSignedURL apiOpts depVersion (vendoredName dependency)
      case response of 
             Left _ -> pure ""
             Right res -> do
                     _ <- Diag.runDiagnostics $ Fossa.archiveUpload res compressedFile
                     pure ""

      pure (Archive (vendoredName dependency) depVersion)

      -- fileHash <- hashFile compressedFile
      -- case fileHash of
      --       Nothing -> traceM "No file hash able to be generated"
      --       Just md5Hash -> do
      --             uri <- mkURI "https://app.fossa.com"
      --             response <- Diag.runDiagnostics $ Fossa.getSignedURL (ApiOpts (Just uri) key) (T.pack $ show md5Hash) packageName
      --             case response of
      --                   Left err -> traceM $ show err
      --                   Right res -> do
      --                         _ <- traceM $ show (Diag.resultValue res)
      --                         arcUpload <- Diag.runDiagnostics $ Fossa.archiveUpload (Diag.resultValue res) compressedFile
      --                         case arcUpload of
      --                               Left err -> traceM $ show err
      --                               Right arc -> traceM $ show $ Diag.resultValue arc



archiveUploadSourceUnit :: FilePath -> ApiOpts -> [VendoredDependency] -> IO (Maybe SourceUnit)
archiveUploadSourceUnit baseDir apiOpts vendoredDeps = do
      archives <- withSystemTempDirectory "fossa-temp" (uploadArchives apiOpts vendoredDeps baseDir)
      
      arcUpload <- Diag.runDiagnostics $ Fossa.archiveBuildUpload apiOpts (ArchiveComponents archives)
      case arcUpload of
            Left err -> traceM $ show err
            Right arc -> traceM $ show arc

      pure $ Just $ archivesToSourceUnit archives

      -- Need to programmatically obtain this
      -- where dir = fromAbsDir testDir <> "user-deps-test"

archivesNoUploadSourceUnit :: [VendoredDependency] -> Maybe SourceUnit
archivesNoUploadSourceUnit deps = Just $ archivesToSourceUnit (unsafeVendoredToArchive <$> deps)

unsafeVendoredToArchive :: VendoredDependency -> Archive
unsafeVendoredToArchive dep = Archive (vendoredName dep) (fromMaybe "" $ vendoredVersion dep)


archivesToSourceUnit :: [Archive] -> SourceUnit
archivesToSourceUnit arcs = do
      let build = toBuildData arcs
          srcUnit = SourceUnit
            { sourceUnitName = "arc deps",
              sourceUnitManifest = "arc deps",
              sourceUnitType = "archive-uploaded-dependencies",
              sourceUnitBuild = Just build,
              additionalData = Nothing
            }

      srcUnit

toBuildData :: [Archive] -> SourceUnitBuild
toBuildData deps =
  SourceUnitBuild
    { buildArtifact = "default",
      buildSucceeded = True,
      buildImports = imports,
      buildDependencies = map addDepList imports
    }
  where
    imports = map arcToLocator deps

    arcToLocator :: Archive -> Locator
    arcToLocator arc = 
      Locator
        { locatorFetcher = "archive",
          locatorProject = archiveName arc,
          locatorRevision = Just $ archiveVersion arc
        }

    addDepList :: Locator -> SourceUnitDependency
    addDepList loc = SourceUnitDependency loc []

compressFile :: FilePath -> FilePath -> FilePath -> IO FilePath
compressFile outputDir directory fileToTar = do
      let finalFile = outputDir </> fileToTar
      entries <- Tar.pack directory [finalFile]
      BS.writeFile finalFile $ GZip.compress $ Tar.write entries
      pure finalFile


hashFile :: FilePath -> IO (Maybe (Digest MD5))
hashFile fileToHash = do
      fileContent <- BS.readFile fileToHash
      let md5Digest = Just (md5 fileContent)
      case md5Digest of
            Nothing -> pure Nothing
            Just digest -> pure $ Just digest