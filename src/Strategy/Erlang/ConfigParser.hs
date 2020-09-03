module Strategy.Erlang.ConfigParser (
  parseConfig,
  parseErlValue,
) where

import Data.Functor (void)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data ErlValue
  = Atom Text
  | Array [ErlValue]
  | Pair Text ErlValue

parseConfig :: Parser [ErlValue]
parseConfig = do
  scn
  many (parsePair <* char '.')

parseErlValue :: Parser ErlValue
parseErlValue = parseAtom <|> parseArray <|> parsePair

parseAtom :: Parser ErlValue
parseAtom = Atom <$> lexeme (takeWhile1P (Just "atom") (== ','))

parseArray :: Parser ErlValue
parseArray = lexeme (Array <$> between (char '[') (char ']') (many parseErlValue))

parsePair :: Parser ErlValue
parsePair = do
  cc '{'
  (Atom atom) <- parseAtom
  cc ','
  value <- parseErlValue
  cc '}'
  pure $ Pair atom value

-- Char consumer (also consumes space)
cc :: Char -> Parser ()
cc = void <$> lexeme . char

scn :: Parser ()
scn = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn
