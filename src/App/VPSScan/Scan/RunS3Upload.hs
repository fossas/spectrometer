module App.VPSScan.Scan.RunS3Upload
  (execS3Upload
  )
where

import qualified Aws
import qualified Aws.S3 as S3
import           Control.Monad.Trans.Resource
-- import           Data.Conduit ((.|), runConduit)
-- import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (newManager, Manager, tlsManagerSettings, RequestBody(..))
import System.Posix.Files
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Control.Monad.IO.Class
-- import Control.Concurrent
-- import System.Posix.Files

import System.IO
import Control.Applicative
import qualified Data.Text as T
import App.VPSScan.Types
import Control.Carrier.Error.Either
import Control.Carrier.Trace.Printing
import Control.Effect.Diagnostics
-- import System.Process.Typed as PROC
-- import System.FilePath (joinPath)
import Data.Text.Prettyprint.Doc (pretty)
-- import qualified Data.Text as T
-- import Effect.Exec
import Prologue


data S3UploadError = ErrorRunningS3Upload Text
  deriving (Eq, Ord, Show, Generic, Typeable)

instance ToDiagnostic S3UploadError where
  renderDiagnostic = \case
    ErrorRunningS3Upload err -> "Error while uploading to S3: " <> pretty err

execS3Upload :: (Has Trace sig m, Has Diagnostics sig m, MonadIO m) => Path Abs Dir -> Text -> IPROpts -> m ()
execS3Upload basedir scanId IPROpts{..} = do
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  mgr <- liftIO $ newManager tlsManagerSettings
  let filePath = "install.sh"
  liftIO $ uploadFileToS3 cfg s3cfg mgr filePath scanId

uploadFileToS3 :: Aws.Configuration -> S3.S3Configuration Aws.NormalQuery -> Manager -> String -> Text -> IO ()
uploadFileToS3 cfg s3cfg mgr filePath scanId =
  runResourceT $ do
    let key :: Text
        key = scanId <> "foo1/install1"
    -- streams large file content, without buffering more than 10k in memory
    let streamer sink = withFile filePath ReadMode $ \h -> sink $ S.hGet h 10240
    size <- liftIO $ (fromIntegral . fileSize <$> getFileStatus filePath :: IO Integer)
    let body = RequestBodyStream (fromInteger size) streamer
    rsp <- Aws.pureAws cfg s3cfg mgr $
        (S3.putObject "scott-s3-upload-test" key body)
      { S3.poMetadata =
        [ ("mediatype", "texts")
        , ("meta-description", "test Internet Archive item made via haskell aws library")
        ]
      -- Automatically creates bucket on IA if it does not exist,
      -- and uses the above metadata as the bucket's metadata.
      , S3.poAutoMakeBucket = True
      }
    -- trace $ show rsp
    pure ()
-- execS3Upload :: (Has Trace sig m, Has Diagnostics sig m, MonadIO m) => Path Abs Dir -> Text -> IPROpts -> m ()
-- execS3Upload basedir scanId IPROpts{..} = do
--   trace $ "Uploading to S3 with this command: " ++ commandString
--   (exitcode, stdout, stderr) <- PROC.readProcess (setWorkingDir (fromAbsDir basedir) (PROC.shell commandString))
--   case (exitcode, stdout, stderr) of
--     (ExitSuccess, out, _) -> do
--       trace $ "stdout: " ++ show out
--       pure ()
--     (_, _, err) -> fatal (ErrorRunningS3Upload (T.pack (show err)))
--   where
--     s3Location = joinPath [s3Url, (T.unpack scanId)]
--     cdPart = "cd " ++ fromAbsDir basedir
--     uploadPart = " | sed 's/^\\.\\///' | shuf | xargs -n 1 -P 100 -I{} aws s3 cp {} " ++ s3Location ++ "/{} --quiet"
--     findFilesPart = "{ find . -maxdepth 1 -mindepth 1 -type f; find . -maxdepth 2 -mindepth 2 -type f; find . -maxdepth 3 -mindepth 3 -type f; }"
--     findDirsPart = "find . -maxdepth 3 -mindepth 3 -type d"
--     commandString = cdPart ++ " && " ++ findFilesPart ++ uploadPart ++ " && " ++ findDirsPart ++ uploadPart ++ " --recursive"