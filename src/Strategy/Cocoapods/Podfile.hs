{-# LANGUAGE RecordWildCards #-}

module Strategy.Cocoapods.Podfile
  ( discover
  , analyze
  , analyze'
  , buildGraph
  , parsePodfile

  , Pod (..)
  , Podfile (..)
  , PropertyType (..)
  ) where

import Control.Effect.Diagnostics
import Data.Functor (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Path
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case findFileNamed "Podfile" files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "cocoapods-podfile" CocoapodsGroup $ analyze file

  pure WalkContinue

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsParser parsePodfile file

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = buildGraph <$> readContentsParser parsePodfile file

mkProjectClosure :: Path Abs File -> Podfile -> ProjectClosureBody
mkProjectClosure file podfile = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph podfile
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

buildGraph :: Podfile -> Graphing Dependency
buildGraph podfile = Graphing.fromList (map toDependency direct)
    where
    direct = pods podfile
    toDependency Pod{..} =
      Dependency { dependencyType = PodType
                 , dependencyName = name
                 , dependencyVersion = case version of
                                            Nothing -> Nothing
                                            Just ver -> Just (CEq ver)
                 , dependencyLocations = case M.lookup SourceProperty properties of
                                            Just repo -> [repo]
                                            _ -> [source podfile]
                 , dependencyEnvironments = []
                 , dependencyTags = M.empty
                 }

type Parser = Parsec Void Text

data Pod = Pod
     { name      :: Text
     , version   :: Maybe Text
     , properties :: Map PropertyType Text
     } deriving (Eq, Ord, Show)

data PropertyType = GitProperty | CommitProperty | SourceProperty | PathProperty
  deriving (Eq, Ord, Show)

data Podfile = Podfile
      { pods :: [Pod]
      , source :: Text
      } deriving (Eq, Ord, Show)

data Line =
      PodLine Pod
      | SourceLine Text
      deriving (Eq, Ord, Show)

parsePodfile :: Parser Podfile
parsePodfile = linesToPodfile (Podfile [] "") . concat <$> ((try podParser <|> findSource <|> ignoredLine) `sepBy` eol) <* eof

linesToPodfile :: Podfile -> [Line] -> Podfile
linesToPodfile file (PodLine pod : xs) = linesToPodfile (file { pods = pod : pods file }) xs
linesToPodfile file (SourceLine sourceLine : xs) = linesToPodfile (file { source = sourceLine }) xs
linesToPodfile file [] = file

findSource :: Parser [Line]
findSource = do
          _ <- chunk "source \'"
          source <- takeWhileP (Just "source parser") (/= '\'')
          _ <- char '\''
          pure [SourceLine source]

podParser :: Parser [Line]
podParser = do
  sc
  _   <- symbol "pod"
  name <- stringLiteral
  version <- optional (try (comma *> stringLiteral))
  properties <- many property
  _ <- restOfLine
  pure [PodLine $ Pod name version (M.fromList properties)]

comma :: Parser ()
comma = () <$ symbol ","

property :: Parser (PropertyType, Text)
property = do
  comma
  propertyType <- choice
    [ GitProperty    <$ symbol ":git"
    , CommitProperty <$ symbol ":commit"
    , SourceProperty <$ symbol ":source"
    , PathProperty   <$ symbol ":path"
    ]
  _ <- symbol "=>"
  value <- stringLiteral
  pure (propertyType, value)

symbol :: Text -> Parser Text
symbol = lexeme . chunk

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

stringLiteral :: Parser Text
stringLiteral = T.pack <$> go
  where
  go = (char '"'  *> manyTill L.charLiteral (char '"'))
   <|> (char '\'' *> manyTill L.charLiteral (char '\''))

sc :: Parser ()
sc =  L.space (void $ some (char ' ')) (L.skipLineComment "#") empty

restOfLine :: Parser Text
restOfLine = takeWhileP (Just "ignored") (not . isEndLine)

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _    = False

ignoredLine :: Parser [Line]
ignoredLine = do
  _ <- restOfLine
  pure []
