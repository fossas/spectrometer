module App.VPSScan.Scan.RunS3Upload
  (execS3Upload
  )
where

import qualified Aws
import qualified Aws.S3 as S3
import qualified Aws.Core as AwsCore
import Control.Monad.Trans.Resource
import Control.Effect.Lift
import Network.HTTP.Conduit (newManager, Manager, tlsManagerSettings, RequestBody(..))
import Path.IO (listDirRecur, isSymlink)
import Control.Monad.Except
import qualified Data.ByteString as S
import Control.Carrier.TaskPool
import Effect.Logger
import GHC.Conc.Sync (getNumCapabilities)
import Text.URI
import qualified Data.Text.Encoding as TE

import System.IO
import Control.Applicative
import qualified Data.Text as T
import Control.Carrier.Trace.Printing
import App.VPSScan.Types
import Control.Effect.Diagnostics
import Prologue

data S3UploadError = ErrorRunningS3Upload Text
  deriving (Eq, Ord, Show, Generic, Typeable)

instance ToDiagnostic S3UploadError where
  renderDiagnostic = \case
    ErrorRunningS3Upload err -> "Error while uploading to S3: " <> pretty err

-- S3.S3Configuration is defined here:
-- https://github.com/aristidb/aws/blob/9ccfce8c50723e1b7e02ba59a9d38227e938ac0a/Aws/S3/Core.hs#L69
s3config :: IPROpts -> S3.S3Configuration Aws.NormalQuery
s3config IPROpts{..} =
  case s3Endpoint of
    Nothing -> baseConfig
    Just endpoint ->
      case uriAuthority endpoint of
        Left _ -> baseConfig
        Right Authority{..} -> do
          baseConfig { S3.s3RequestStyle = requestStyle, S3.s3Port = port, S3.s3Endpoint = TE.encodeUtf8 host , S3.s3Protocol = protocol }
          where
            host = unRText authHost
            protocol = case uriScheme endpoint of
              Nothing -> AwsCore.HTTP
              Just s ->
                if (T.toLower $ unRText s) == "https" then
                  AwsCore.HTTPS
                else
                  AwsCore.HTTP
            port = case authPort of
              Nothing -> case protocol of
                AwsCore.HTTPS -> 443
                AwsCore.HTTP -> 80
              Just p -> fromEnum p
            requestStyle = case host of
              "s3.amazonaws.com" -> S3.BucketStyle
              _ -> S3.PathStyle


    where
      baseConfig = Aws.defServiceConfig

execS3Upload :: (Has Diagnostics sig m, MonadIO m, Has Trace sig m) => Path Abs Dir -> Text -> IPROpts -> String -> m ()
execS3Upload basedir scanId opts@IPROpts{..} bucket = do
  cfg <- Aws.baseConfiguration
  let s3cfg = s3config opts

  mgr <- liftIO $ newManager tlsManagerSettings
  trace "[S3] Finding files to upload"
  -- listDirRecur returns a list of all of the files in a directory recursively, but doesn't recurse down symlinked dirs
  (_, allFilesAndSymlinks) <- listDirRecur basedir
  -- listDirRecur returns symlinked files, which we don't want, so get rid of them before uploading
  allFiles <- filterM (fmap not . isSymlink) allFilesAndSymlinks
  trace $ "[S3] " ++ show (length allFiles) ++ " files found. Starting upload to S3"

  capabilities <- liftIO getNumCapabilities
  trace $ "[S3] Uploading to S3 at " ++ bucket ++ "/" ++ T.unpack scanId ++ " in " ++ show capabilities ++ " threads"

  _ <- liftIO $ withLogger SevTrace $ withTaskPool capabilities updateProgress $ traverse_ (forkTask . uploadAbsFilePath cfg s3cfg mgr (T.pack bucket) basedir scanId) allFiles
  pure ()

updateProgress :: Has Logger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )

uploadAbsFilePath :: (Has (Lift IO) sig m) => Aws.Configuration -> S3.S3Configuration Aws.NormalQuery -> Manager -> Text -> Path Abs Dir -> Text -> Path Abs File -> m ()
uploadAbsFilePath    cfg s3cfg mgr bucketName basedir scanId filepath =
  case stripProperPrefix basedir filepath of
    Nothing -> pure ()
    Just relPath -> do
      let key = scanId <> "/" <> T.pack (fromRelFile relPath)
      _ <- sendIO $ runResourceT $ do
        -- streams large file content, without buffering more than 10k in memory
        let streamer sink = withFile (fromAbsFile filepath) ReadMode $ \h -> sink $ S.hGet h 10240
        size <- liftIO $ withFile (fromAbsFile filepath) ReadMode hFileSize
        let body = RequestBodyStream (fromInteger size) streamer
        Aws.pureAws cfg s3cfg mgr (S3.putObject bucketName key body)
      pure ()