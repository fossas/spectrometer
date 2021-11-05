module Strategy.Python.SetupPy (
  analyze',
) where

import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (Dependency)
import Effect.ReadFS (ReadFS, readContentsParser)
import Graphing (Graphing)
import Path (Abs, File, Path)
import Strategy.Python.Util (Req, buildGraph, requirementParser)
import Text.Megaparsec (
  Parsec,
  anySingle,
  between,
  sepEndBy,
  skipManyTill,
  (<|>),
 )
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer qualified as L

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do
  reqs <- readContentsParser installRequiresParser file
  context "Building dependency graph" $ pure (buildGraph reqs)

type Parser = Parsec Void Text

installRequiresParser :: Parser [Req]
installRequiresParser = prefix *> entries <* end
  where
    prefix = skipManyTill anySingle (symbol "install_requires") *> symbol "=" *> symbol "["
    entries = (requireSurroundedBy "\"" <|> requireSurroundedBy "\'") `sepEndBy` symbol ","

    requireSurroundedBy :: Text -> Parser Req
    requireSurroundedBy quote = between (symbol quote) (symbol quote) requirementParser

    end = symbol "]"

    symbol :: Text -> Parser Text
    symbol = L.symbol space
