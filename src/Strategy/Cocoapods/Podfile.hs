module Strategy.Cocoapods.Podfile
  ( discover
  , strategy
  , analyze
  , configure
  , parsePodfile

  , Pod (..)
  , Podfile (..)
  , Location (..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Char as C
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           DepTypes
import           Discovery.Walk
import           Effect.LabeledGrapher
import           Effect.ReadFS
import           Graphing (Graphing, unfold)
import           Types
import           Text.Megaparsec hiding (label)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

discover :: Discover
discover = Discover
  { discoverName = "podfile"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  for_ files $ \f ->
    when (fileName f == "Podfile") $ output (configure f)
  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "podfile"
  , strategyAnalyze = \opts -> analyze & fileInputParser parsePodfile (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts

analyze :: Member (Input Podfile) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

buildGraph :: Podfile -> Graphing Dependency
buildGraph podfile = unfold direct (const []) toDependency
    where
    direct = pods podfile
    toDependency Pod{..} =
      Dependency { dependencyType = PodType
               , dependencyName = name
               , dependencyVersion = case version of
                                          Nothing -> Nothing
                                          Just name -> Just (CEq name)
               , dependencyLocations = case location of 
                                          Just (Source repo) -> [repo]
                                          _ -> [source podfile]
               , dependencyTags = M.empty
               }

type Parser = Parsec Void Text

data Pod = Pod
     { name      :: Text
     , version   :: Maybe Text
     , location  :: Maybe Location
     } deriving (Eq, Ord, Show, Generic)

type Repo = Text
type Version = Text
type Directory = Text

data Podfile = Podfile
      { pods :: [Pod]
      , source :: Text
      } deriving (Eq, Ord, Show, Generic)

data Location =
      Git Repo (Maybe Version)
      | Source Repo
      | Path Directory
      deriving (Eq, Ord, Show, Generic)

data Line =
      PodLine Pod
      | SourceLine Text
      deriving (Eq, Ord, Show, Generic)

parsePodfile :: Parser Podfile
parsePodfile = (linesToPodfile $ Podfile [] "") . concat <$> ((podParser <|> findSource <|> ignoredLine) `sepBy` eol) <* eof

linesToPodfile :: Podfile -> [Line] -> Podfile
linesToPodfile file (PodLine pod : xs) = linesToPodfile (file { pods = pod : pods file }) xs
linesToPodfile file (SourceLine sourceLine : xs) = linesToPodfile (file { source = sourceLine }) xs
linesToPodfile file [] = file

findSource :: Parser [Line]
findSource = do
          _ <- chunk "source \'"
          source <- takeWhileP (Just "source") (/= '\'')
          _ <- char '\''
          pure [SourceLine source]

podParser :: Parser [Line]
podParser = do
          _ <- chunk "  pod " <|> chunk "pod "
          dep <- findDep
          _<- optional $ chunk ", "
          version <- optional findVersion
          _<- optional $ chunk ", "
          remote <- optional $ gitParser <|> pathParser <|> sourceParser
          _ <- restOfLine
          pure [PodLine $ Pod dep version remote]

findDep :: Parser Text
findDep = do
      _ <- char '\'' <|> char '\"'
      result <- takeWhileP (Just "dependency") (\a -> a /= '\'' && a /= '\"')
      _ <- char '\'' <|> char '\"'
      pure result

findVersion :: Parser Text
findVersion = do
      _ <- chunk "\'"
      result <- takeWhileP (Just "version") (/= '\'')
      _ <- char '\''
      pure result

gitParser :: Parser Location
gitParser = do
      _ <- chunk ":git => \'"
      project <- takeWhileP (Just "git remote") (/= '\'')
      version <- optional (commitParser <|> tagParser)
      _ <- char '\''
      pure $ Git project version

commitParser :: Parser Text
commitParser = do
      _ <- chunk "\', :commit => \'"
      version <- takeWhileP (Just "commit parser") (/= '\'')
      pure version

tagParser :: Parser Text
tagParser = do
      _ <- chunk "\', :tag => \'"
      version <- takeWhileP (Just "tag parser") (/= '\'')
      pure version

pathParser :: Parser Location
pathParser = do
      _ <- chunk ":path => \'"
      project <- takeWhileP (Just "path") (/= '\'')
      _ <- char '\''
      pure $ Path project

sourceParser :: Parser Location
sourceParser = do
      c <- chunk ":source => \'"
      _ <- traceM $ show c
      repo <- takeWhileP (Just "path") (/= '\'')
      _ <- char '\''
      pure $ Source repo

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