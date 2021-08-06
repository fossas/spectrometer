module Strategy.Elixir.ElixirParser (
  ElixirValue (..),
  ElixirText (..),
  ElixirAccessor (..),
  ElixirBool (..),
  ElixirAtom (..),
  ElixirKeyword (..),
  parseElixirAtom,
  parseElixirText,
  parseElixirValue,
  parseComma,
  betweenCurlyBrackets,
  betweenSquareBrackets,
  lexeme,
  symbol,
  findKeyword,
  findKeywordWithStringValue,
) where

import Control.Monad (void)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (takeWhile1P, try),
  Parsec,
  between,
  chunk,
  empty,
  sepEndBy,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  Lexer.space
    (void $ some (char ' ' <|> char '\r' <|> char '\n' <|> char '\t'))
    -- elixir uses # for line comments
    (Lexer.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

betweenQuotation :: Parser t -> Parser t
betweenQuotation = between (symbol "\"") (lexeme $ symbol "\"")

betweenCurlyBrackets :: Parser t -> Parser t
betweenCurlyBrackets = between (lexeme $ symbol "{") (lexeme $ symbol "}")

betweenSquareBrackets :: Parser t -> Parser t
betweenSquareBrackets = between (lexeme $ symbol "[") (lexeme $ symbol "]")

parseComma :: Parser Text
parseComma = lexeme $ chunk ","

-- # Elixir Basic Data Types Parsing
-- Reference: https://elixir-lang.org/getting-started/basic-types.html

data ElixirValue
  = EString ElixirText
  | Atom ElixirAtom
  | EBool ElixirBool
  | Accessor ElixirAccessor
  | Keyword ElixirKeyword
  | List [ElixirValue]
  | Tuple [ElixirValue]
  deriving (Show, Eq, Ord)

newtype ElixirText = ElixirText {getText :: Text} deriving (Show, Eq, Ord)
newtype ElixirAtom = ElixirAtom {getAtom :: Text} deriving (Show, Eq, Ord)
newtype ElixirAccessor = ElixirAccessor {getAccessor :: Text} deriving (Show, Eq, Ord)
newtype ElixirBool = ElixirBool {getBool :: Bool} deriving (Show, Eq, Ord)
data ElixirKeyword = ElixirKeyword {getKey :: ElixirAccessor, getValue :: ElixirValue} deriving (Show, Eq, Ord)

parseElixirBool :: Parser ElixirBool
parseElixirBool =
  ElixirBool . strToBool
    <$> lexeme
      ( chunk "true"
          <|> chunk "false"
          <|> chunk ":true"
          <|> chunk ":false"
      )
  where
    strToBool :: Text -> Bool
    strToBool "true" = True
    strToBool ":true" = True
    strToBool "false" = False
    strToBool ":false" = False
    strToBool _ = False

parseElixirText :: Parser ElixirText
parseElixirText = ElixirText <$> betweenQuotation (takeWhile1P (Just "") (/= '"'))

parseElixirAtom :: Parser ElixirAtom
parseElixirAtom = ElixirAtom . toText <$> lexeme (chunk ":" *> some (alphaNumChar <|> char '_'))

parseElixirAccessor :: Parser ElixirAccessor
parseElixirAccessor = ElixirAccessor . toText <$> lexeme (some (alphaNumChar <|> char '_') <* chunk ":")

parseElixirKeyword :: Parser ElixirKeyword
parseElixirKeyword = lexeme $ do
  accessor <- parseElixirAccessor
  ElixirKeyword accessor <$> parseElixirValue

parseElixirTuple :: Parser ElixirValue
parseElixirTuple = Tuple <$> betweenCurlyBrackets (sepEndBy (parseElixirValue) (lexeme $ symbol ","))

parseElixirList :: Parser ElixirValue
parseElixirList = List <$> betweenSquareBrackets (sepEndBy (parseElixirValue) (lexeme $ symbol ","))

parseElixirValue :: Parser ElixirValue
parseElixirValue =
  lexeme $
    try (EBool <$> parseElixirBool)
      <|> try (EString <$> parseElixirText)
      <|> try (Keyword <$> parseElixirKeyword)
      <|> try (Atom <$> parseElixirAtom)
      <|> try (Accessor <$> parseElixirAccessor)
      <|> try parseElixirTuple
      <|> parseElixirList

-- | Finds keyword's value recursively, if predicate matches.
findKeyword :: (ElixirAccessor -> Bool) -> [ElixirValue] -> Maybe ElixirValue
findKeyword f (Keyword x : xs) =
  if f (getKey x)
    then Just $ getValue x
    else findKeyword f ([getValue x]) <|> findKeyword f xs
findKeyword f (Tuple x : xs) = findKeyword f x <|> findKeyword f xs
findKeyword f (List x : xs) = findKeyword f x <|> findKeyword f xs
findKeyword f (_ : xs) = findKeyword f xs
findKeyword _ [] = Nothing

-- | Finds keyword's value recursively, based on accessor's text value.
findKeywordWithStringValue :: [ElixirValue] -> Text -> Maybe Text
findKeywordWithStringValue opts lookupKey = case findKeyword (\x -> getAccessor x == lookupKey) opts of
  Just (EString (ElixirText source)) -> Just source
  _ -> Nothing
