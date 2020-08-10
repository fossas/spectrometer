module App.VPSScan.Scan.RunSherlock
  ( execSherlock
  , SherlockOpts(..)
  )
where

import App.VPSScan.Types
import App.VPSScan.Scan.Core
import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import qualified Data.Text as T
import Effect.Exec
import Prologue

data SherlockOpts = SherlockOpts
  { scanDir :: Path Abs Dir
  , scanId :: Text
  , clientToken :: Text
  , clientId :: Text
  , sherlockUrl :: Text
  , organizationId :: Int
  , projectId :: Locator
  , revisionId :: Text
  , sherlockVpsOpts :: VPSOpts
  } deriving (Generic)

execSherlock :: (Has Exec sig m, Has Diagnostics sig m) => FilePath -> SherlockOpts -> m ()
execSherlock binaryPath sherlockOpts = void $ execThrow (scanDir sherlockOpts) (sherlockCommand binaryPath sherlockOpts)

sherlockCommand :: FilePath -> SherlockOpts -> Command
sherlockCommand binaryPath SherlockOpts{..} = do
  let VPSOpts{..} = sherlockVpsOpts

  Command
    { cmdName = T.pack binaryPath,
      cmdArgs =
        [ "scan", T.pack $ fromAbsDir scanDir,
          "--scan-id", scanId,
          "--sherlock-api-secret-key", clientToken,
          "--sherlock-api-client-id", clientId,
          "--sherlock-api-host", sherlockUrl,
          "--organization-id", T.pack $ show organizationId,
          "--project-id", unLocator projectId,
          "--revision-id", revisionId,
          "--filter-expressions", unFilterExpressions filterBlob
        ],
      cmdAllowErr = Never
    }
