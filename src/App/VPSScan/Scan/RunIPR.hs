{-# LANGUAGE OverloadedStrings #-}

module App.VPSScan.Scan.RunIPR
  ( IPROpts (..),
    IPR (..),
    IPRC (..),
    execIPR,
    IPRError (..),
    iprShouldRun,
    validateIPROpts,
    ValidatedIPROpts,
  )
where

import qualified Data.HashMap.Strict as HM
import Control.Carrier.Error.Either
import qualified Data.Text as T
import qualified Data.Vector as V
import Prologue
import Effect.Exec

data IPROpts = IPROpts
  { maybeIprCmdPath :: Maybe String,
    maybeNomosCmdPath :: Maybe String,
    maybePathfinderCmdPath :: Maybe String,
    iprEnabled :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

data ValidatedIPROpts = ValidatedIPROpts
  { iprCmdPath :: String,
    nomosCmdPath :: String,
    pathfinderCmdPath :: String
  }
  deriving (Eq, Ord, Show, Generic)

validateIPROpts :: IPROpts -> Maybe ValidatedIPROpts
validateIPROpts IPROpts{..} =
  case (maybeIprCmdPath, maybeNomosCmdPath, maybePathfinderCmdPath, iprEnabled) of
    (Just iprPath, Just nomosPath, Just pathfinderPath, True) -> Just (ValidatedIPROpts iprPath nomosPath pathfinderPath)
    _ -> Nothing

iprShouldRun :: IPROpts -> Bool
iprShouldRun iprOpts =
  case validateIPROpts iprOpts of
    Just _ -> True
    Nothing -> False

iprCmdArgs :: Path Abs Dir -> ValidatedIPROpts -> [String]
iprCmdArgs baseDir ValidatedIPROpts {..} = ["-target", toFilePath baseDir, "-nomossa", nomosCmdPath, "-pathfinder", pathfinderCmdPath]

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

----- ipr effect

data IPRError
  = NoFilesEntryInOutput
  | IPRCommandFailed Text
  deriving (Eq, Ord, Show, Generic)

data IPR m k where
  ExecIPR :: Path Abs Dir -> ValidatedIPROpts -> IPR m (Either IPRError Value)

execIPR :: Has IPR sig m => Path Abs Dir -> ValidatedIPROpts -> m (Either IPRError Value)
execIPR basedir iprOpts = send (ExecIPR basedir iprOpts)

----- production ipr interpreter

newtype IPRC m a = IPRC {runIPR :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (IPR :+: sig) (IPRC m) where
  alg hdl sig ctx = IPRC $ case sig of
    R other -> alg (runIPR . hdl) other ctx
    L (ExecIPR basedir opts@ValidatedIPROpts {..}) -> do
      let iprCommand :: Command
          iprCommand = Command [iprCmdPath] [] Never

      maybeValue <- runError @ExecErr $ runExecIO $ execJson basedir iprCommand $ iprCmdArgs basedir opts
      let result = case maybeValue of
            Left err -> (Left (IPRCommandFailed (T.pack (show err))))
            Right value -> do
              let maybeExtracted = extractNonEmptyFiles value
              case maybeExtracted of
                Nothing -> (Left NoFilesEntryInOutput)
                Just extracted -> (Right extracted)

      pure (result <$ ctx)
