{-# LANGUAGE OverloadedStrings #-}

module VPSScan.RunIPR
  ( scan
  , IPROpts(..)
  , IprResponse(..)
  , IprFile(..)
  , IprLicenseExpression(..)
  , IprLicense(..)

  , IPR(..)
  , IPRC(..)
  , execIPR
  , IPRError(..)
  ) where

import VPSScan.ScotlandYard
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Prologue

import Control.Carrier.Error.Either
import Effect.Exec
import Network.HTTP.Req (HttpException)

data IPROpts = IPROpts
  { iprCmdPath :: String
  , nomosCmdPath :: String
  , pathfinderCmdPath :: String
  } deriving (Eq, Ord, Show, Generic)

scan :: (Has Exec sig m, Has (Error ExecErr) sig m, Has (Error HttpException) sig m, MonadIO m) => Path Abs Dir -> Text -> ScotlandYardOpts -> IPROpts -> m ()
scan baseDir scanId scotlandYardOpts@ScotlandYardOpts{..} opts@IPROpts{..} = do
  let c :: [String]
      c = [iprCmdPath]
      iprCommand :: Command
      iprCommand = Command c [] Never

  (value :: Value) <- execJson baseDir iprCommand $ iprCmdArgs opts

  let maybeExtracted = extractNonEmptyFiles value
  case maybeExtracted of
    Nothing -> throwError (CommandParseError (T.pack iprCmdPath) "Couldn't extract files from command output")
    Just extracted -> do
      res <- runHTTP (postIprResults scotlandYardOpts scanId extracted)
      case res of
        Left err -> throwError err
        Right a -> pure a

iprCmdArgs :: IPROpts -> [String]
iprCmdArgs IPROpts{..} = ["-nomossa", nomosCmdPath, "-pathfinder", pathfinderCmdPath]

extractNonEmptyFiles :: Value -> Maybe Array
extractNonEmptyFiles (Object obj) = do
  files <- HM.lookup "Files" obj
  filesAsArray <- case files of
    Array filesArray -> Just filesArray
    _ -> Nothing

  let filtered = V.filter hasLicenses filesAsArray

      hasLicenses :: Value -> Bool
      hasLicenses (Object file) =
        case HM.lookup "LicenseExpressions" file of
          Just (Object expressions) -> not (HM.null expressions)
          _ -> False
      hasLicenses _ = False

  pure filtered
extractNonEmptyFiles _ = Nothing

-- mkFileList :: IprResponse -> IprResponse
-- mkFileList iprOutput = IprResponse iprOutput

data IprResponse = IprResponse { iprResponseFiles :: [IprFile] } deriving (Eq, Ord, Show, Generic)

data IprFile = IprFile 
  { iprFilePath :: String
  , iprFileLicenseExpressions :: Map String IprLicenseExpression
  -- , iprFileCopyrights :: [IprCopyright]
  -- , iprFileAssociations :: [IprAssociation]
  } deriving (Eq, Ord, Show, Generic)

data IprLicenseExpression = IprLicenseExpression
  { iprLicenseExpressionInstanceID :: Int
  , iprLicenseExpressionExpression :: String
  , iprLicenseExpressionLicenses :: [IprLicense]
  , iprLicenseExpressionDeclaration :: Maybe String
  , iprLicenseExpressionVerbatim :: String
  , iprLicenseExpressionOffsetStart :: Int
  , iprLicenseExpresseionOffsetEnd :: Int
  } deriving (Eq, Ord, Show, Generic)

data IprLicense = IprLicense
  { iprLicenseId :: String 
  -- , iprLicenseNotes :: String
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON IprResponse where
  parseJSON = withObject "IprResponse" $ \obj ->
    IprResponse <$> obj .: "Files"

instance FromJSON IprFile where
  parseJSON = withObject "IprFile" $ \obj ->
    IprFile <$> obj .: "Path"
            <*> obj .: "LicenseExpressions"

-- in this case, obj is a HashMap with a key we don't care about
-- and a value that contains the data for license expressiosn
instance FromJSON IprLicenseExpression where
  parseJSON = withObject "IprLicenseExpression" $ \obj ->
      IprLicenseExpression <$> obj .: "LicenseExpressionInstanceID"
                           <*> obj .: "Expression"
                           <*> obj .: "Licenses"
                           <*> obj .: "Declaration"
                           <*> obj .: "Verbatim"
                           <*> obj .: "OffsetStart"
                           <*> obj .: "OffsetEnd"

instance FromJSON IprLicense where
  parseJSON = withObject "IprLicense" $ \obj ->
    IprLicense <$> obj .: "ID"
              --  <*> obj .: "Notes"

----- ipr effect

data IPRError
  = NoFilesEntryInOutput
  | IPRCommandFailed Text
  deriving (Eq, Ord, Show, Generic)

data IPR m k
  = ExecIPR (Path Abs Dir) IPROpts (Either IPRError Array -> m k)
  deriving Generic1

instance HFunctor IPR
instance Effect IPR

execIPR :: Has IPR sig m => Path Abs Dir -> IPROpts -> m (Either IPRError Array)
execIPR basedir iprOpts = send (ExecIPR basedir iprOpts pure)

----- production ipr interpreter
 
newtype IPRC m a = IPRC { runIPR :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m, Effect sig) => Algebra (IPR :+: sig) (IPRC m) where
  alg (R other) = IPRC (alg (handleCoercible other))
  alg (L (ExecIPR basedir opts@IPROpts{..} k)) = (k =<<) . IPRC $ do
    let iprCommand :: Command
        iprCommand = Command [iprCmdPath] [] Never

    maybeValue <- runError @ExecErr $ runExecIO $ execJson basedir iprCommand $ iprCmdArgs opts
    case maybeValue of
      Left err -> pure (Left (IPRCommandFailed (T.pack (show err))))
      Right value -> do
        let maybeExtracted = extractNonEmptyFiles value
        case maybeExtracted of
          Nothing -> pure (Left NoFilesEntryInOutput)
          Just extracted -> pure (Right extracted)
