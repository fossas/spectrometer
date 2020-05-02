module App.VPSScan.Types
(VPSOpts(..),
SherlockOpts(..),
ScotlandYardOpts(..))
where

import Prologue
import qualified App.VPSScan.Scan.RunIPR as RunIPR
import OptionExtensions (UrlOption)

data ScotlandYardOpts = ScotlandYardOpts
  { scotlandYardUrl :: UrlOption }

data SherlockOpts = SherlockOpts
  { sherlockCmdPath :: String
  , sherlockUrl :: String
  , sherlockClientToken :: String
  , sherlockClientID :: String
  } deriving (Eq, Ord, Show, Generic)

data VPSOpts = VPSOpts
  { vpsSherlock :: SherlockOpts
  , vpsIpr :: RunIPR.IPROpts
  , vpsScotlandYard :: ScotlandYardOpts
  , organizationID :: Int
  , projectID :: Text
  , revisionID :: Text
  }
