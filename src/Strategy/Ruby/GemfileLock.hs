
module Strategy.Ruby.GemfileLock
  ( discover
  , strategy
  , analyze
  , configure
  , findSections

  , Spec(..)
  , SpecDep(..)
  , DirectDep(..)
  , Section(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Char as C
import           Polysemy
import           Polysemy.Input
import           Polysemy.Output

import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
import           Effect.ReadFS
import qualified Graph as G
import           Types
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug

discover :: Discover
discover = Discover
  { discoverName = "gemfilelock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  for_ files $ \f ->
    when (fileName f == "Gemfile.lock") $ output (configure f)
  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "gemfile-lock"
  , strategyAnalyze = \opts -> analyze & fileInputParser findSections (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts

type Remote = Text
type Revision = Text
type Branch = Text

data Section =
      GitSection Remote (Maybe Revision) (Maybe Branch) [Spec]
      | GemSection Remote [Spec]
      | PathSection Remote [Spec]
      | DependencySection [DirectDep]
      | UnknownSection Text
        deriving (Eq, Ord, Show, Generic)

data Spec = Spec
      { specVersion :: Text
      , specName :: Text
      , specDeps :: [SpecDep]
      } deriving (Eq, Ord, Show, Generic)

newtype SpecDep = SpecDep
      { depName :: Text
      } deriving (Eq, Ord, Show, Generic)

newtype DirectDep = DirectDep
      { directName :: Text
      } deriving (Eq, Ord, Show, Generic)

analyze :: Member (Input [Section]) r => Sem r G.Graph
analyze = buildGraph <$> input

buildGraph :: [Section] -> G.Graph
buildGraph sections = run $ evalGraphBuilder G.empty $ do
      let depMap = buildNodes sections
      nodeMap <- traverse addNode depMap
      buildEdges sections nodeMap
      buildDirect sections nodeMap

buildNodes :: [Section] -> Map Text G.Dependency
buildNodes sections = M.fromList $ concatMap depsFromSection sections

depsFromSection :: Section-> [(Text, G.Dependency)]
depsFromSection (GitSection _ _ _ specs) = map (\a -> (specName a, depFromSpec a)) specs 
depsFromSection (GemSection _ specs) = map (\a -> (specName a, depFromSpec a)) specs 
depsFromSection (PathSection _ specs) = map (\a -> (specName a, depFromSpec a)) specs 
depsFromSection _ = []
      
depFromSpec :: Spec -> G.Dependency
depFromSpec x = G.Dependency G.GemType (specName x) (Just $ G.CEq $ specVersion x) [] M.empty

-- Populate the dependencies with their deep deps
buildEdges :: Member GraphBuilder r => [Section] -> Map Text G.DepRef -> Sem r ()
buildEdges sections depGraph = traverse_ (buildSectionEdges depGraph) sections

buildSectionEdges :: Member GraphBuilder r => Map Text G.DepRef -> Section -> Sem r ()
buildSectionEdges depGraph (GitSection _ _ _ specs) = buildSpecsEdges specs depGraph
buildSectionEdges depGraph (GemSection _ specs) = buildSpecsEdges specs depGraph
buildSectionEdges depGraph (PathSection _ specs) = buildSpecsEdges specs depGraph
buildSectionEdges _ _ = pure ()

buildSpecsEdges :: Member GraphBuilder r => [Spec] -> Map Text G.DepRef -> Sem r ()
buildSpecsEdges specs depGraph = traverse_ (buildSpecEdges depGraph) specs

buildSpecEdges :: Member GraphBuilder r => Map Text G.DepRef -> Spec -> Sem r ()
buildSpecEdges depMap (Spec _ name children) = traverse_ (buildSpecDepEdge depMap name) children

buildSpecDepEdge :: Member GraphBuilder r => Map Text G.DepRef -> Text -> SpecDep -> Sem r ()
buildSpecDepEdge depMap name dep = do
      let refs = do
            parentRef <- M.lookup name depMap
            childRef <- M.lookup (depName dep) depMap
            Just (parentRef, childRef)
      case refs of
            Just (parentRef, childRef) -> addEdge parentRef childRef 
            Nothing -> pure ()

buildDirect :: Member GraphBuilder r => [Section] -> Map Text G.DepRef -> Sem r ()
buildDirect sections depMap = traverse_ (directDepsFromSection depMap) sections

directDepsFromSection :: Member GraphBuilder r =>  Map Text G.DepRef -> Section -> Sem r ()
directDepsFromSection dependencyNameMap (DependencySection specs)  = traverse_ (addDirectDep dependencyNameMap) specs
directDepsFromSection _ _ = pure ()

addDirectDep :: Member GraphBuilder r =>  Map Text G.DepRef -> DirectDep -> Sem r ()
addDirectDep depMap x = 
      case M.lookup (directName x) depMap of
            Just directRef -> addDirect directRef
            Nothing -> pure ()

type Parser = Parsec Void Text

findSections :: Parser [Section]
findSections = many (try gitSectionParser <|> try gemSectionParser <|> try pathSectionParser <|> try dependenciesSectionParser <|> emptySection) <* eof

emptySection :: Parser Section
emptySection = do 
      emptyLine <- restOfLine
      _ <- eol
      pure $ UnknownSection emptyLine
      
restOfLine :: Parser Text
restOfLine = takeWhileP (Just "ignored") (not . isEndLine)

-- ignore content until the end of the line
ignored :: Parser ()
ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

gitSectionParser :: Parser Section
gitSectionParser = mkSectionParser "GIT" $ \propertyMap -> do
  remote <- lookupRawText "remote" propertyMap
  specs <- lookupRawSpecs "specs" propertyMap
  let revision = eitherToMaybe $ lookupRawText "revision" propertyMap
      branch = eitherToMaybe $ lookupRawText "branch" propertyMap
  pure $ GitSection remote revision branch specs

pathSectionParser :: Parser Section
pathSectionParser = mkSectionParser "PATH" $ \propertyMap -> do
  remote <- lookupRawText "remote" propertyMap
  specs <- lookupRawSpecs "specs" propertyMap
  pure $ PathSection remote specs

gemSectionParser :: Parser Section
gemSectionParser = mkSectionParser "GEM" $ \propertyMap -> do
  remote <- lookupRawText "remote" propertyMap
  specs <- lookupRawSpecs "specs" propertyMap
  pure $ GemSection remote specs

mkSectionParser :: Text -> (Map Text RawField -> Either Text Section) -> Parser Section
mkSectionParser sectionName toSection = L.nonIndented scn (L.indentBlock scn p)
      where
         p = do
            _ <- chunk sectionName
            pure $ L.IndentMany Nothing propertiesToSection (try propertyParser <|> specPropertyParser)

         propertiesToSection :: [(Text, RawField)] -> Parser Section
         propertiesToSection properties =
            let propertyMap = M.fromList properties
                result :: Either Text Section
                result = toSection propertyMap

            in case result of
                  Right x -> pure x
                  Left y -> fail $ T.unpack $ "could not parse " <> sectionName <> " section: " <> y

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _) = Nothing

lookupRawText :: Text -> Map Text RawField -> Either Text Text
lookupRawText key m = let value = M.lookup key m
                  in
                  case value of
                        Just (RawText val) -> Right val
                        _ -> Left $ "a value for " <> key <> " was unable to be found in the map"

lookupRawSpecs :: Text -> Map Text RawField -> Either Text [Spec]
lookupRawSpecs key m = let value = M.lookup key m
                  in
                  case value of
                        Just (RawSpecs val) -> Right val
                        _ -> Left $ "a value for " <> key <> " was unable to be found in the map"

data RawField = RawText Text
                | RawSpecs [Spec]
                deriving (Eq, Ord, Show, Generic)

propertyParser :: Parser (Text, RawField)
propertyParser = do
      remote <- findFieldName
      _ <- chunk ":"
      value <- textValue
      pure (remote, value)

      where 
            findFieldName :: Parser Text
            findFieldName = takeWhileP (Just "field name") (/= ':')

            textValue :: Parser RawField
            textValue = do
                  _ <- chunk " "
                  RawText <$> restOfLine

specPropertyParser :: Parser (Text, RawField)
specPropertyParser = L.indentBlock scn p
      where 
      p = do
            remote <- findFieldName
            _ <- chunk ":"
            pure $ L.IndentMany Nothing (\a -> pure (remote, RawSpecs a)) specParser
            
      findFieldName :: Parser Text
      findFieldName = takeWhileP (Just "field name") (/= ':')

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _    = False

specParser :: Parser Spec
specParser = L.indentBlock scn p 
  where
    p = do
      name <- findDep
      version <- findVersion
      return (L.IndentMany Nothing (\specs -> pure $ Spec version name specs) specDepParser)

specDepParser :: Parser SpecDep
specDepParser = do
      name <- findDep
      _ <- ignored
      pure $ SpecDep name

scn :: Parser ()
scn =  L.space space1 empty empty

sc :: Parser ()
sc =  L.space (void $ some (char ' ')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

findDep :: Parser Text
findDep = lexeme (takeWhile1P (Just "dep") (not . C.isSpace))

findVersion :: Parser Text
findVersion = do
      _ <- char '('
      result <- lexeme (takeWhileP (Just "version") (/= ')'))
      _ <- char ')'
      pure result


dependenciesSectionParser :: Parser Section
dependenciesSectionParser = L.nonIndented scn (L.indentBlock scn p)
                  where
                        p = do
                              _ <- chunk "DEPENDENCIES"
                              pure $ L.IndentMany Nothing (\deps -> pure $ DependencySection deps) findDependency

findDependency :: Parser DirectDep
findDependency = do
      dep <- findDep
      _ <- ignored
      pure $ DirectDep dep
