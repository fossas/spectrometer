-- | Module      : Strategy.Xcode.PbxprojParser
--
-- Provides elementary parsing of xcode's pbxproj.project file.
-- Xcode uses plist ascii, encoded in UTF-8 to perform record configurations.
--
-- There is no official spec, for the file format.
--
-- It can represents data in:
--  * Binary
--  * Date
--  * String
--  * List
--  * Dictionary
--
-- Relevant References:
--   * For ASCII types: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/PropertyLists/OldStylePlists/OldStylePLists.html
--   * Various `pbxproj.project` format's articles (unofficial):
--     * http://www.monobjc.net/xcode-project-file-format.html
--
-- We intentionally parse all types into one of String, List, and Dictionary.
-- We do not distinguish between types of "configuration" or "xcode specific property type".
module Strategy.Xcode.PbxprojParser (
  parsePbxProj,
  PbxProj (..),
  AsciiValue (..),

  -- * for testing only
  parseAsciiText,
  parseAsciiList,
  parseAsciiDict,
  parseAsciiValue,
) where

import Data.Functor (void)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (takeWhile1P, try),
  Parsec,
  between,
  many,
  noneOf,
  sepEndBy,
  some,
  (<?>),
  (<|>),
 )
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  Lexer.space
    (void $ some $ char ' ' <|> char '\t' <|> char '\n' <|> char '\r')
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

betweenCurlyBrackets :: Parser a -> Parser a
betweenCurlyBrackets = between (symbol "{") (symbol "}")

betweenParentheses :: Parser a -> Parser a
betweenParentheses = between (symbol "(") (symbol ")")

parseQuotedText :: Parser Text
parseQuotedText = between (symbol "\"") (symbol "\"") quoted
  where
    quoted :: Parser Text
    quoted = toText <$> many (nullifiedQuote <|> notEscapedQuote)

    nullifiedQuote :: Parser Char
    nullifiedQuote = string "\\\"" >> pure '"'

    notEscapedQuote :: Parser Char
    notEscapedQuote = noneOf ['\"']

parseText :: Parser Text
parseText = takeWhile1P (Just "text") (\c -> c `notElem` [';', ',', ')', ' ', '\t', '\n', '\r'])

-- | Potential type represented in Ascii plist file.
-- Reference : https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/PropertyLists/OldStylePlists/OldStylePLists.html
data AsciiValue
  = -- | Represents SomeText or "SomeText"
    -- Since we are only interested in textual representation of package name, and package version
    -- We represent potential binary, date, boolean ascii type as text.
    AText Text
  | -- | Represents {key = value;}
    ADict (Map Text AsciiValue)
  | -- | Represents (A, B,)
    AList [AsciiValue]
  deriving (Show, Eq, Ord)

data AsciiKeyValue = AsciiKeyValue Text AsciiValue deriving (Show, Eq, Ord)

parseAsciiText :: Parser AsciiValue
parseAsciiText = AText <$> lexeme (try parseQuotedText <|> parseText)

parseAsciiList :: Parser AsciiValue
parseAsciiList = AList <$> betweenParentheses (sepEndBy parseAsciiValue (symbol ","))

parseAsciiValue :: Parser AsciiValue
parseAsciiValue = try parseAsciiDict <|> try parseAsciiList <|> parseAsciiText

parseAsciiDict :: Parser AsciiValue
parseAsciiDict = ADict <$> (Map.fromList <$> lexeme (betweenCurlyBrackets $ sepEndBy (try parseAsciiKeyValue) (symbol ";")))

parseAsciiKeyValue :: Parser (Text, AsciiValue)
parseAsciiKeyValue = do
  key <- lexeme parseText <* symbol "="
  value <- lexeme $ try parseAsciiList <|> try parseAsciiDict <|> parseAsciiText
  pure (key, value)

-- | Represents Xcode's pbxproj.project file elementary structure.
-- Reference:
data PbxProj = PbxProj
  { archiveVersion :: Text
  , objectVersion :: Text
  , rootObject :: Text
  , classes :: Maybe AsciiValue
  , objects :: Maybe AsciiValue
  }
  deriving (Show, Eq, Ord)

(|->) :: AsciiValue -> Text -> Maybe AsciiValue
(|->) (AText _) _ = Nothing
(|->) (AList _) _ = Nothing
(|->) (ADict val) key = Map.lookup key val

textOf :: AsciiValue -> Maybe Text
textOf (AText t) = Just t
textOf _ = Nothing

supportedEncoding :: Text
supportedEncoding = "UTF8"

parsePbxProj :: Parser PbxProj
parsePbxProj = do
  _ <- symbol ("// !$*" <> supportedEncoding <> "*$!") <?> "to have UTF8 Encoding!"
  allValues <- parseAsciiDict

  archiveVersion <- case (textOf =<< (allValues |-> "archiveVersion")) of
    Nothing -> fail "could not find, archiveVersion"
    Just av -> pure av

  objectVersion <- case (textOf =<< (allValues |-> "objectVersion")) of
    Nothing -> fail "could not find, objectVersion"
    Just ov -> pure ov

  rootObject <- case (textOf =<< (allValues |-> "rootObject")) of
    Nothing -> fail "could not find, rootObject"
    Just ro -> pure ro

  let classes = (allValues |-> "classes")
  let objects = (allValues |-> "objects")
  pure $ PbxProj archiveVersion objectVersion rootObject classes objects
