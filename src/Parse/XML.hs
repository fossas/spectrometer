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

  -- * Helper functions
  , defaultsTo

  -- * Error formatting
  , xmlErrorPretty
  ) where

import Prelude

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Polysemy
import Polysemy.Error
import Polysemy.NonDet
import Polysemy.Reader
import qualified Text.XML.Light as XML

class FromXML a where
  parseElement :: XML.Element -> Parser a

instance FromXML XML.Element where
  parseElement = pure

instance FromXML T.Text where
  parseElement = content

instance FromXML v => FromXML (M.Map T.Text v) where
  parseElement el = M.fromList <$> traverse mkSingle (XML.elChildren el)
    where
    mkSingle e = do
      let key :: T.Text
          key = T.pack (XML.qName (XML.elName e))
      value <- parseElement e
      pure (key, value)

-- NonDet and Fail are required for Alternative/MonadFail/MonadPlus instances.
-- We interpret in terms of `Error`, where non-throwing branches are returned.
newtype Parser a = Parser { unParser :: Sem '[Reader ParsePath, NonDet, Error ParseError] a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance MonadFail Parser where
  fail msg = Parser $ ask >>= \path -> throw (UnknownError (Just path) msg)

runParser :: String -> Parser a -> Either ParseError a
runParser rootName = run
  . runError
  . nonDetToError (UnknownError Nothing "NonDet.empty")
  . runReader [rootName]
  . unParser

data ParseError =
    ParseElementMissing ParsePath String
  | ParseAttrMissing ParsePath String
  | ParseXMLDocFailed
  | UnknownError (Maybe ParsePath) String
  deriving (Eq, Ord, Show)

xmlErrorPretty :: ParseError -> T.Text
xmlErrorPretty (ParseElementMissing path childName) =
  "Missing child at " <> renderPath path <> "; childName: " <> T.pack childName
xmlErrorPretty (ParseAttrMissing path attrName) =
  "Missing attribute at " <> renderPath path <> "; attrName: " <> T.pack attrName
xmlErrorPretty (UnknownError path err) =
  "UnknownError at " <> renderPath (fromMaybe [] path) <> "; err: " <> T.pack err
xmlErrorPretty ParseXMLDocFailed = "parseXMLDoc failed"

renderPath :: ParsePath -> T.Text
renderPath [] = "[no path]"
renderPath xs = (\path -> "[" <> path <> "]") . T.intercalate "." . map T.pack . reverse $ xs

type ParsePath = [String] -- reversed parse path

parseXML :: FromXML a => T.Text -> Either ParseError a
parseXML inp =
  case XML.parseXMLDoc inp of
    Nothing -> Left ParseXMLDocFailed
    Just root -> runParser (XML.qName (XML.elName root)) (parseElement root)

attr :: String -> XML.Element -> Parser T.Text
attr attrName el =
  case XML.findAttrBy (\elName -> XML.qName elName == attrName) el of
    Nothing -> Parser $ ask >>= \path -> throw (ParseAttrMissing path attrName)
    Just a  -> pure (T.pack a)

child :: FromXML a => String -> XML.Element -> Parser a
child childName el =
  case XML.filterChildName (\elName -> XML.qName elName == childName) el of
    Nothing -> Parser $ ask >>= \path -> throw (ParseElementMissing path childName)
    Just a  -> subparse childName a

children :: FromXML a => String -> XML.Element -> Parser [a]
children name = traverse (subparse name) . XML.filterChildrenName (\elName -> XML.qName elName == name)

content :: XML.Element -> Parser T.Text
content = Parser . pure . T.pack . XML.strContent

-- default an optional field to a specific value
defaultsTo :: Parser (Maybe a) -> a -> Parser a
defaultsTo fa a = fmap (fromMaybe a) fa

-- parse a child element, and add its name to the parse path
subparse :: FromXML a => String -> XML.Element -> Parser a
subparse path el = Parser $ local (path:) (unParser (parseElement el))
