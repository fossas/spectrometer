{-# LANGUAGE OverloadedStrings #-}

module App.VPSScan.Scan.RunIPR
  ( execIPR
  , IPRError (..)
  , IPROpts (..)
  )
where

import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Effect.Exec
import Data.Aeson
import qualified Data.Text as T
import Path
import Prelude
import App.VPSScan.Types
import App.VPSScan.EmbeddedBinary
import GHC.Generics (Generic)

extractNonEmptyFiles :: Value -> Maybe Value
extractNonEmptyFiles (Object obj) = do
  files <- HM.lookup "Files" obj
  filesAsArray <- case files of
    Array filesArray -> Just filesArray
    _ -> Nothing

  let filtered = V.filter hasLicensesOrCopyrights filesAsArray
      hasLicenses :: Value -> Bool
      hasLicenses (Object file) =
        case HM.lookup "LicenseExpressions" file of
          Just (Object expressions) -> not (HM.null expressions)
          _ -> False
      hasLicenses _ = False
      hasCopyrights :: Value -> Bool
      hasCopyrights (Object file) =
        case HM.lookup "Copyrights" file of
          Just (Object expressions) -> not (HM.null expressions)
          _ -> False
      hasCopyrights _ = False
      hasLicensesOrCopyrights :: Value -> Bool
      hasLicensesOrCopyrights value = hasLicenses value || hasCopyrights value

  Just $ object ["Files" .= filtered]
extractNonEmptyFiles _ = Nothing

data IPRError = NoFilesEntryInOutput
  deriving (Eq, Ord, Show)

instance ToDiagnostic IPRError where
  renderDiagnostic NoFilesEntryInOutput = "No \"Files\" entry in the IPR output"

data IPROpts = IPROpts
  { scanDir :: Path Abs Dir
  , iprVpsOpts :: VPSOpts
  } deriving (Generic)

execIPR :: (Has Exec sig m, Has Diagnostics sig m) => IPRBinaryPaths -> IPROpts -> m Value
execIPR iprPaths iprOpts = do
  value <- execJson (scanDir iprOpts) (iprCommand iprPaths iprOpts)
  let maybeExtracted = extractNonEmptyFiles value
  case maybeExtracted of
    Nothing -> fatal NoFilesEntryInOutput
    Just extracted -> pure extracted

iprCommand :: IPRBinaryPaths -> IPROpts -> Command
iprCommand IPRBinaryPaths{..} IPROpts{..} = do
  let VPSOpts{..} = iprVpsOpts
  Command
    { cmdName = T.pack ramjetBinaryPath,
      cmdArgs = [
        "-target", ".",
        "-nomossa", T.pack nomosBinaryPath,
        "-pathfinder", T.pack pathfinderBinaryPath,
        "-filter-expressions", unFilterExpressions filterBlob
      ],
      cmdAllowErr = Never
    }
