module Parse.XML
  ( -- * Types
    FromXML(..)
  , Parser(..)
  , parseXML

  -- * Parsing an XML element
  , attr
  , child
  , children
  , content

  -- * Error formatting
  , xmlErrorPretty
  ) where

import Prelude

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fail (MonadFail)
import qualified Data.Text as T
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.NonDet
import Polysemy.Reader
import qualified Text.XML.Light as XML

class FromXML a where
  parseElement :: XML.Element -> Parser a

instance FromXML T.Text where
  parseElement = fmap T.pack . content

-- NonDet and Fail are required for Alternative/MonadFail/MonadPlus instances.
-- We interpret in terms of `Error`, where non-throwing branches are returned.
newtype Parser a = Parser { unParser :: Sem '[Reader ParsePath, Fail, NonDet, Error ParseError] a }
  deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus)

data ParseError =
    ParseElementMissing ParsePath String
  | ParseAttrMissing ParsePath String
  | UnknownError String

xmlErrorPretty :: ParseError -> T.Text
xmlErrorPretty (ParseElementMissing path childName) =
  "Missing child at " <> renderPath path <> "; childName: " <> T.pack childName
xmlErrorPretty (ParseAttrMissing path attrName) =
  "Missing attribute at " <> renderPath path <> "; attrName: " <> T.pack attrName
xmlErrorPretty (UnknownError err) =
  "UnknownError " <> T.pack err

renderPath :: ParsePath -> T.Text
renderPath = T.intercalate "." . map T.pack . reverse

type ParsePath = [String] -- reversed parse path

parseXML :: FromXML a => XML.Element -> Either ParseError a
parseXML = run
  . runError
  . nonDetToError (UnknownError "NonDet.empty")
  . failToNonDet
  . runReader []
  . unParser
  . parseElement

attr :: String -> XML.Element -> Parser String
attr attrName el =
  case XML.findAttrBy (\elName -> XML.qName elName == attrName) el of
    Nothing -> Parser $ ask >>= \path -> throw (ParseAttrMissing path attrName)
    Just a  -> pure a

child :: FromXML a => String -> XML.Element -> Parser a
child childName el = Parser $
  case XML.filterChildName (\elName -> XML.qName elName == childName) el of
    Nothing -> ask >>= \path -> throw (ParseElementMissing path childName)
    Just a  -> local (childName:) (unParser (parseElement a))

children :: FromXML a => String -> XML.Element -> Parser [a]
children name = traverse (subparse name) . XML.filterChildrenName (\elName -> XML.qName elName == name)

content :: XML.Element -> Parser String
content = Parser . pure . XML.strContent

subparse :: FromXML a => String -> XML.Element -> Parser a
subparse path el = Parser $ local (path:) (unParser (parseElement el))
