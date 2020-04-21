module VPSScan.ScotlandYard (createScan, postIprResults, ScotlandYardOpts(..))
where
import Prologue

import Control.Carrier.Error.Either
import Effect.HTTP
import qualified Data.Text as T

data ScotlandYardOpts = ScotlandYardOpts
  { scotlandYardUrl :: String
  , organizationID :: String
  , projectID :: String
  , revisionID :: String
  } deriving (Eq, Ord, Show, Generic)

createScan :: (Has (Error HTTPErr) sig m) => Maybe ScotlandYardOpts -> m String
createScan opts = do
  case opts of 
    Nothing -> throwError (NoUrlError "No URL provided for Scotland Yard API")
    Just _ -> do
      -- TODO: Actually make the post
      return "1234"

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IRP result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
postIprResults :: (Has (Error HTTPErr) sig m) => Maybe String -> String -> String -> m ()
postIprResults scotlandYardUri resJSON scanId = do
  case scotlandYardUri of
    Nothing -> throwError (NoUrlError $ T.pack $ "No URL provided for Scotland Yard API: " ++ resJSON ++ scanId)
    Just _ -> do
      pure ()
