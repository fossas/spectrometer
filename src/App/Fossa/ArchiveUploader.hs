{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module App.Fossa.ArchiveUploader
  ( archiveUploadSourceUnit,
    archivesNoUploadSourceUnit, 
  )
where


import Debug.Trace
import Control.Effect.Lift
import Control.Effect.Path (withSystemTempDir)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import Path hiding ((</>))
-- import Path
import Data.Text (Text)
import App.Fossa.YamlDeps
import qualified Data.Text as T
import Crypto.Hash
import qualified App.Fossa.FossaAPIV1 as Fossa
import Fossa.API.Types
import qualified Control.Carrier.Diagnostics as Diag
import System.FilePath.Posix
import Srclib.Types (Locator (..), SourceUnit (..), SourceUnitBuild (..), SourceUnitDependency (SourceUnitDependency))
import Data.Maybe (fromMaybe)

uploadArchives :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => ApiOpts -> [VendoredDependency] -> Path Abs Dir -> Path Abs Dir -> m [Archive]
uploadArchives apiOpts deps arcDir tmpDir = traverse (compressAndUpload apiOpts arcDir tmpDir) deps

compressAndUpload ::(Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => ApiOpts -> Path Abs Dir -> Path Abs Dir -> VendoredDependency -> m Archive
compressAndUpload apiOpts arcDir tmpDir dependency = do
      compressedFile <- sendIO $ compressFile tmpDir arcDir (T.unpack $ vendoredPath dependency)

      traceM $ show compressedFile
      depVersion <- case vendoredVersion dependency of
                      Nothing -> sendIO $ hashFile compressedFile
                      Just version -> pure version

      traceM $ show depVersion
      signedURL <- Fossa.getSignedURL apiOpts depVersion (vendoredName dependency)

      _ <- Fossa.archiveUpload signedURL compressedFile

      pure $ Archive (vendoredName dependency) depVersion

archiveUploadSourceUnit :: (Has Diag.Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> ApiOpts -> [VendoredDependency] -> m (Maybe SourceUnit)
archiveUploadSourceUnit baseDir apiOpts vendoredDeps = do
      archives <- withSystemTempDir "fossa-temp" (uploadArchives apiOpts vendoredDeps baseDir)

      -- archiveBuildUpload takes archives without Organization information. This orgID is appended when creating the build on the backend.
      _ <- Fossa.archiveBuildUpload apiOpts (ArchiveComponents archives)

      Fossa.Organization orgId _ <- Fossa.getOrganization apiOpts 
      let archivesWithOrganization = updateArcName (T.pack $ show orgId) <$> archives

      pure $ Just $ archivesToSourceUnit archivesWithOrganization

      where
        updateArcName :: Text -> Archive -> Archive
        updateArcName updateText arc = arc {archiveName = updateText <> "/" <> archiveName arc}

archivesNoUploadSourceUnit :: [VendoredDependency] -> Maybe SourceUnit
archivesNoUploadSourceUnit deps = Just $ archivesToSourceUnit (unsafeVendoredToArchive <$> deps)

unsafeVendoredToArchive :: VendoredDependency -> Archive
unsafeVendoredToArchive dep = Archive (vendoredName dep) (fromMaybe "" $ vendoredVersion dep)

archivesToSourceUnit :: [Archive] -> SourceUnit
archivesToSourceUnit arcs = do
      let build = toBuildData arcs
          srcUnit = SourceUnit
            { sourceUnitName = "archive deps",
              sourceUnitManifest = "archive deps",
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

compressFile :: Path Abs Dir -> Path Abs Dir -> FilePath -> IO FilePath
compressFile outputDir directory fileToTar = do
      let finalFile = fromAbsDir outputDir </> fileToTar
      traceM $ show finalFile
      entries <- Tar.pack (fromAbsDir directory) [fileToTar]
      BS.writeFile finalFile $ GZip.compress $ Tar.write entries
      pure finalFile

md5 :: BS.ByteString  -> Digest MD5
md5 = hashlazy

hashFile :: FilePath -> IO Text
hashFile fileToHash = do
      fileContent <- BS.readFile fileToHash
      pure . T.pack . show $ md5 fileContent