{-# LANGUAGE QuasiQuotes #-}

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

import Data.Char qualified as C
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
  oneOf,
  sepEndBy,
  some,
  (<?>),
  (<|>),
 )
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- XCode uses ASCII property list format.
-- Apple Reference: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/PropertyLists/OldStylePlists/OldStylePLists.html
--
-- String: "some string"
-- Data: <0fbd777 1c2735ae>
-- Array: ("awesome", "weather")
-- Dictionary: { user = superadmin; birth = 1564; }
--
-- Other References:
-- - http://www.monobjc.net/xcode-project-file-format.html

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

parseQuotedText :: Parser Text
parseQuotedText = between (symbol "\"") (symbol "\"") (toText <$> many (escapedChar <|> normalChar))
  where
    escapedChar =
      (char '\\')
        *> ( oneOf
              [ '\\'
              , '"'
              , 'n'
              , 'r'
              , 't'
              , 'v'
              , '\''
              , 'f'
              , 'e'
              , 'a'
              ]
           )
    normalChar = noneOf ['\"']

parseText :: Parser Text
parseText = takeWhile1P (Just "text") isNotSpaceOrColonOrComma

isNotSpaceOrColonOrComma :: Char -> Bool
isNotSpaceOrColonOrComma c = (c /= ';' && c /= ',' && c /= ')') && (not . C.isSpace) c

data AsciiValue
  = AText Text
  | ADict (Map Text AsciiValue)
  | AList [AsciiValue]
  deriving (Show, Eq, Ord)

data AsciiKeyValue = AsciiKeyValue Text AsciiValue deriving (Show, Eq, Ord)

parseAsciiText :: Parser AsciiValue
parseAsciiText = AText <$> lexeme (try parseQuotedText <|> parseText)

parseAsciiList :: Parser AsciiValue
parseAsciiList = do
  _ <- symbol "("
  val <- AList <$> sepEndBy (parseAsciiValue) (symbol ",")
  _ <- symbol ")"
  pure val

parseAsciiValue :: Parser AsciiValue
parseAsciiValue = try parseAsciiDict <|> try parseAsciiList <|> parseAsciiText

parseAsciiDict :: Parser AsciiValue
parseAsciiDict = ADict <$> (Map.fromList <$> lexeme (betweenCurlyBrackets $ sepEndBy (try parseAsciiKeyValue) (symbol ";")))

parseAsciiKeyValue :: Parser (Text, AsciiValue)
parseAsciiKeyValue = do
  key <- lexeme parseText <* symbol "="
  value <- lexeme $ try parseAsciiList <|> try parseAsciiDict <|> parseAsciiText
  pure (key, value)

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