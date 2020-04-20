module VPSScan.ScotlandYard (createScan, postIprResults)
where
import Prologue

import Control.Carrier.Error.Either
import Effect.HTTP

createScan :: (Has (Error HTTPErr) sig m) => Maybe String -> m String
createScan scotlandYardUri = do
  case scotlandYardUri of 
    Nothing -> throwError (NoUrlError "No URL provided for Scotland Yard API")
    Just uri -> do
      -- TODO: Actually make the post
      return "1234"

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IRP result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
postIprResults :: (Has (Error HTTPErr) sig m) => Maybe String -> String -> String -> m ()
postIprResults scotlandYardUri resJSON scanId = do
  case scotlandYardUri of
    Nothing -> throwError (NoUrlError "No URL provided for Scotland Yard API")
    Just uri -> do
      pure ()
