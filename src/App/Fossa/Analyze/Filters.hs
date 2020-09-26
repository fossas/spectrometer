module App.Fossa.Analyze.Filters
  ( BuildTargetFilter (..),
    filterParser,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Path
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data BuildTargetFilter
  = -- | buildtool, directory
    ProjectFilter Text (Path Rel Dir)
  | -- | buildtool, directory, buildtarget
    TargetFilter Text (Path Rel Dir) Text
  deriving (Eq, Ord, Show)

filterParser :: Parser BuildTargetFilter
filterParser = (try targetFilter <|> projectFilter) <* eof
  where
    targetFilter =
      TargetFilter <$> buildtool <* char '@' <*> path <* char ':' <*> target
    projectFilter =
      ProjectFilter <$> buildtool <* char '@' <*> path

    buildtool :: Parser Text
    buildtool = T.pack <$> some alphaNumChar

    path :: Parser (Path Rel Dir)
    path = do
      filepath <- some (satisfy (/= ':'))
      case parseRelDir filepath of
        Left err -> fail (show err)
        Right a -> pure a

    target :: Parser Text
    target = takeWhile1P Nothing (const True)
