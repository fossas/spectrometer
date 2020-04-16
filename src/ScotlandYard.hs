module ScotlandYard (createScan, postIprResults)
where
import Prologue

import Control.Carrier.Error.Either
import Effect.Exec

createScan :: (Has Exec sig m, Has (Error ExecErr) sig m) => Maybe String -> m String
createScan scotlandYardUri = do
  case scotlandYardUri of 
    Nothing -> return ""
    Just uri -> do
      -- TODO: Actually make the post
      pure "1234"

postIprResults :: (Has Exec sig m, has (Error ExecErr) sig m) => String -> String -> m ()
postIprResults resJSON scanId = do
  pure ()