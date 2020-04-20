{-# LANGUAGE OverloadedStrings #-}

module VPSScan.RunIPR ( scan, IprOpts, IprResponse(..), IprFile(..), IprLicenseExpression(..) ) where
import Prologue
import qualified Data.HashMap.Strict as HM
import Data.Aeson.Types

import Control.Carrier.Error.Either
import Effect.Exec

data IprOpts = IprOpts
  { baseDir :: Path Abs Dir
  , iprCmdPath :: String
  , nomosCmdPath :: String
  , pathfinderCmdPath :: String
  } deriving (Eq, Ord, Show, Generic)

scan :: (Has Exec sig m, Has (Error ExecErr) sig m) => IprOpts -> m IprResponse
scan IprOpts{..} = do
  let c :: [String]
      c = [iprCmdPath]
      iprCommand :: Command
      iprCommand = Command c [] Never
  execJson baseDir iprCommand [nomosCmdPath, pathfinderCmdPath]

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
  -- , iprLicenseExpressionLicenses :: [IprLicense]
  -- , iprLicenseExpressionDeclaration :: String
  -- , iprLicenseExpressionVerbatim :: String
  -- , iprLicenseExpressionOffsetStart :: Int
  -- , iprLicenseExpresseionOffsetEnd :: Int
  } deriving (Eq, Ord, Show, Generic)

data IprLicense = IprLicense
  { iprLicenseId :: String 
  , iprLicenseNotes :: String
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON IprResponse where
  parseJSON = withObject "IprResponse" $ \obj ->
    IprResponse <$> obj .: "Files"

instance FromJSON IprFile where
  parseJSON = withObject "IprFile" $ \obj ->
    IprFile <$> obj .: "Path"
            <*> obj .: "LicenseExpressions"
            -- <*> parseLicenseExpressions obj 

-- parseLicenseExpressions :: Object -> Parser [IprLicenseExpression]
-- parseLicenseExpressions = 
--   for (HM.toList objs) $ \(_, obj) ->
--     parseJSON @IprLicenseExpression obj

-- in this case, obj is a HashMap with a key we don't care about
-- and a value that contains the data for license expressiosn
instance FromJSON IprLicenseExpression where
  parseJSON = withObject "IprLicenseExpression" $ \obj ->
      IprLicenseExpression <$> obj .: "LicenseExpressionInstanceID"
                           <*> obj .: "Expression"