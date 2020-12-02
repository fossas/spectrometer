{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan.RunWiggins
  ( execWiggins
  , generateWigginsOpts
  , WigginsOpts(..)
  , ScanType(..)
  )
where

import App.Fossa.VPS.Types
import App.Fossa.VPS.EmbeddedBinary
import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import Effect.Exec
import Path
import Effect.Logger
import Fossa.API.Types
import App.Types
import Text.URI

data ScanType = ScanType
  { scanSkipIpr :: Bool
  , scanLicenseOnly :: Bool
  }

data WigginsOpts = WigginsOpts
  { scanDir :: Path Abs Dir
  , spectrometerArgs :: [Text]
  }

generateWigginsOpts :: Path Abs Dir -> Severity -> OverrideProject -> ScanType -> FilterExpressions -> ApiOpts -> ProjectMetadata -> WigginsOpts
generateWigginsOpts scanDir logSeverity overrideProject scanType fileFilters apiOpts metadata =
  WigginsOpts scanDir (generateSpectrometerArgs logSeverity overrideProject scanType fileFilters apiOpts metadata)

generateSpectrometerArgs :: Severity -> OverrideProject -> ScanType -> FilterExpressions -> ApiOpts -> ProjectMetadata -> [Text]
generateSpectrometerArgs logSeverity OverrideProject{..} ScanType{..} fileFilters ApiOpts{..} ProjectMetadata{..} =
    "analyze"
      : ["-endpoint", renderUri apiOptsUri, "-fossa-api-key", unApiKey apiOptsApiKey]
      ++ optMaybeText "-name" overrideName
      ++ optMaybeText "-revision" overrideRevision
      ++ optMaybeText "-jira-project-key" projectJiraKey
      ++ optMaybeText "-link" projectLink
      ++ optMaybeText "-policy" projectPolicy
      ++ optMaybeText "-project-url" projectUrl
      ++ optMaybeText "-team" projectTeam
      ++ optMaybeText "-title" projectTitle
      ++ optBool "-license-only" scanLicenseOnly
      ++ optBool "-skip-ipr-scan" scanSkipIpr
      ++ optBool "-debug" (logSeverity == SevDebug)
      ++ optFilterExpressions fileFilters
      ++ ["."]

-- (kit) wiggins currently expects `endpoint` to not have a trailing slash. 
-- I'll fix that, but for now, remove any trailing slash from the rendered URI.
renderUri :: URI -> Text
renderUri uri = do
  let rendered = T.unpack $ render uri
  let trimmed = if last rendered == '/' then init rendered else rendered
  T.pack trimmed

optFilterExpressions :: FilterExpressions -> [Text]
optFilterExpressions (FilterExpressions []) = []
optFilterExpressions expressions = ["-i", encodeFilterExpressions expressions]

optBool :: Text -> Bool -> [Text]
optBool flag True = [flag]
optBool _ False = []

optMaybeText :: Text -> Maybe Text -> [Text]
optMaybeText _ Nothing = []
optMaybeText flag (Just value) = [flag, value]

execWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m ()
execWiggins binaryPaths opts = void $ execThrow (scanDir opts) (wigginsCommand binaryPaths opts)

wigginsCommand :: BinaryPaths -> WigginsOpts -> Command
wigginsCommand BinaryPaths{..} WigginsOpts{..} = do
  Command
    { cmdName = T.pack $ fromAbsFile wigginsBinaryPath,
      cmdArgs = spectrometerArgs,
      cmdAllowErr = Never
    }
