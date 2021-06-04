{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module App.Fossa.Compression
where

import Debug.Trace

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import Path hiding ((</>))
import Data.Text (Text)
import qualified Data.Text as T
import Crypto.Hash
import qualified App.Fossa.FossaAPIV1 as Fossa
import Fossa.API.Types
import qualified Control.Carrier.Diagnostics as Diag
import System.FilePath.Posix
import Text.URI (mkURI)
import System.IO.Temp ( withSystemTempDirectory )

md5 :: BS.ByteString  -> Digest MD5
md5 = hashlazy

testDir :: Path Abs Dir
testDir = $(mkAbsDir "/Users/zachlavallee/Programming/Fossa-Tools/spectrometer/")

compressAndUpload :: Text -> FilePath -> FilePath -> FilePath -> IO ()
compressAndUpload packageName arcDir arcFile tmpDir = do
      compressedFile <- compressFile tmpDir arcDir arcFile
      fileHash <- hashFile compressedFile
      case fileHash of
            Nothing -> traceM "No file hash able to be generated"
            Just md5Hash -> do
                  uri <- mkURI "https://app.fossa.com"
                  response <- Diag.runDiagnostics $ Fossa.getSignedURL (ApiOpts (Just uri) key) (T.pack $ show md5Hash) packageName
                  case response of
                        Left err -> traceM $ show err
                        Right res -> do
                              _ <- traceM $ show (Diag.resultValue res)
                              arcUpload <- Diag.runDiagnostics $ Fossa.archiveUpload (Diag.resultValue res) compressedFile
                              case arcUpload of
                                    Left err -> traceM $ show err
                                    Right arc -> traceM $ show $ Diag.resultValue arc
      where
            key = ApiKey "441b262861010aa8a84038d69ed7f201"



fullArchive :: IO ()
fullArchive = do
      withSystemTempDirectory "fossa-temp" (compressAndUpload depName dir file)
      let comps = Fossa.ArchiveComponents [Fossa.Archive depName "12342"]
      uri <- mkURI "https://app.fossa.com"
      arcUpload <- Diag.runDiagnostics $ Fossa.archiveBuildUpload (ApiOpts (Just uri) key) comps
      case arcUpload of
            Left err -> traceM $ show err
            Right arc -> traceM $ show $ Diag.resultValue arc


      where dir = fromAbsDir testDir <> "user-deps-test"
            file = "archive"
            depName = "zach-test-dep-6"
            key = ApiKey "441b262861010aa8a84038d69ed7f201"

compressFile :: FilePath -> FilePath -> FilePath -> IO FilePath
compressFile outputDir directory fileToTar = do
      entries <- Tar.pack directory [fileToTar]
      _ <- traceM $ show outputDir
      _ <- traceM $ show fileToTar
      let finalFile = outputDir </> fileToTar
      _ <- traceM $ show finalFile
      BS.writeFile finalFile $ GZip.compress $ Tar.write entries
      pure finalFile


hashFile :: FilePath -> IO (Maybe (Digest MD5))
hashFile fileToHash = do
      fileContent <- BS.readFile fileToHash
      let md5Digest = Just (md5 fileContent)
      case md5Digest of
            Nothing -> pure Nothing
            Just digest -> pure $ Just digest