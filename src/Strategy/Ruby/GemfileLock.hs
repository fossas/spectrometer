
module Strategy.Ruby.GemfileLock
  ( discover
  , strategy
  , analyze
  , configure
  ) where

import Prologue hiding (many, some)

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
    when (fileName f == "Gemfile.lock") $ do
      output (configure f)
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

data SpecDep = SpecDep
      { depName :: Text
      } deriving (Eq, Ord, Show, Generic)

data DirectDep = DirectDep
      { directName :: Text
      } deriving (Eq, Ord, Show, Generic)

analyze :: Member (Input [Section]) r => Sem r G.Graph
analyze = do 
      sectionList <- input
      pure $ buildGraph sectionList

buildGraph :: [Section] -> G.Graph
buildGraph sections = traceShowId $ run $ evalGraphBuilder G.empty $ do
      let depMap = buildNodes sections
      otherMap <- traverse addNode depMap
      buildEdges sections otherMap
      buildDirect sections otherMap
      

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
buildEdges sections map = traverse_ (buildSectionEdges map) sections

buildSectionEdges :: Member GraphBuilder r => Map Text G.DepRef -> Section -> Sem r ()
buildSectionEdges map (GitSection _ _ _ specs) = addSpecsEdges specs map
buildSectionEdges map (GemSection _ specs) = addSpecsEdges specs map
buildSectionEdges map (PathSection _ specs) = addSpecsEdges specs map
buildSectionEdges _ _ = pure ()

addSpecsEdges :: Member GraphBuilder r => [Spec] -> Map Text G.DepRef -> Sem r ()
addSpecsEdges (x : xs) map = do
      addSpecEdges map (specName x) (specDeps x)
      addSpecsEdges xs map
addSpecsEdges [] _ = pure ()

addSpecEdges :: Member GraphBuilder r => Map Text G.DepRef -> Text -> [SpecDep] -> Sem r ()
addSpecEdges map parent (x : xs) = do
      let refs = do
            parentRef <- M.lookup parent map
            childRef <- M.lookup (depName x) map
            Just (parentRef, childRef)
      case refs of
            Just (parentRef, childRef) -> addEdge parentRef childRef 
            Nothing -> pure ()
      addSpecEdges map parent xs
addSpecEdges _ _ [] = pure ()
      

-- New Work here that finds direct deps from the direct section only.
buildDirect :: Member GraphBuilder r => [Section] -> Map Text G.DepRef -> Sem r ()
buildDirect sections depMap = traverse_ (directDepsFromSection depMap) sections


directDepsFromSection :: Member GraphBuilder r =>  Map Text G.DepRef -> Section -> Sem r ()
directDepsFromSection dependencyNameMap (DependencySection specs)  = traverse_ (addDirectDep dependencyNameMap) specs
directDepsFromSection _ _ = pure ()

addDirectDep :: Member GraphBuilder r =>  Map Text G.DepRef -> DirectDep -> Sem r ()
addDirectDep depMap x = do
      case M.lookup (directName x) depMap of
            Just directRef -> addDirect directRef
            Nothing -> pure ()

-- Parse Gemfile
-- 1. Find top level(?) deps with their remote location, and transitive deps
-- 2. Associate the transitive deps to one another once we get all deps.
-- 3. Make DEPENDENCIES block all direct deps and reference them to the previously discovered deps

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
gitSectionParser = L.nonIndented scn (L.indentBlock scn p)
      where 
         p = do
            _ <- chunk "GIT"
            pure $ L.IndentMany Nothing (propertiesToGitSection) (try propertyParser <|> specPropertyParser)
         propertiesToGitSection :: [(Text, RawField)] -> Parser Section
         propertiesToGitSection properties = 
            let propertyMap = M.fromList properties
                result :: Either Text Section
                result = do
                  remote <- lookupRawText "remote" propertyMap
                  specs <- lookupRawSpecs "specs" propertyMap
                  let revision = eitherToMaybe $ lookupRawText "revision" propertyMap
                      branch = eitherToMaybe $ lookupRawText "branch" propertyMap
                  pure $ GitSection remote revision branch specs
            
            in case result of
                  Right x -> pure x
                  Left y -> fail $ T.unpack $ "could not parse GIT section: " <> y


pathSectionParser :: Parser Section
pathSectionParser = L.nonIndented scn (L.indentBlock scn p)
      where 
         p = do
            _ <- chunk "PATH"
            pure $ L.IndentMany Nothing (propertiesToPathSection) (try propertyParser <|> specPropertyParser)
         propertiesToPathSection :: [(Text, RawField)] -> Parser Section
         propertiesToPathSection properties = 
            let propertyMap = M.fromList properties
                result :: Either Text Section
                result = do
                  remote <- lookupRawText "remote" propertyMap
                  specs <- lookupRawSpecs "specs" propertyMap
                  pure $ PathSection remote specs
            
            in case result of
                  Right x -> pure x
                  Left y -> fail $ T.unpack $ "could not parse PATH section: " <> y

gemSectionParser :: Parser Section
gemSectionParser = L.nonIndented scn (L.indentBlock scn p)
      where 
         p = do
            _ <- chunk "GEM"
            pure $ L.IndentMany Nothing (propertiesToGemSection) (try propertyParser <|> specPropertyParser)
         propertiesToGemSection :: [(Text, RawField)] -> Parser Section
         propertiesToGemSection properties = 
            let propertyMap = M.fromList properties
                result :: Either Text Section
                result = do
                  remote <- lookupRawText "remote" propertyMap
                  specs <- lookupRawSpecs "specs" propertyMap
                  pure $ GemSection remote specs
            
            in case result of
                  Right x -> pure x
                  Left y -> fail $ T.unpack $ "could not parse GEM section: " <> y

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
      pure $ (remote, value)

      where 
            findFieldName :: Parser Text
            findFieldName = takeWhileP (Just "field name") (\a -> a /= ':')

            textValue :: Parser RawField
            textValue = do
                  _ <- chunk " "
                  value <- restOfLine
                  pure $ RawText value

specPropertyParser :: Parser (Text, RawField)
specPropertyParser = L.indentBlock scn p
      where 
      p = do
            remote <- findFieldName
            _ <- chunk ":"
            pure $ L.IndentMany Nothing (\a -> pure (remote, RawSpecs a)) specParser
            
      findFieldName :: Parser Text
      findFieldName = takeWhileP (Just "field name") (\a -> a /= ':')

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _    = False

specParser :: Parser (Spec)
specParser = L.indentBlock scn p 
  where
    p = do
      name <- findDep
      version <- findVersion
      return (L.IndentMany Nothing (\a -> pure $ Spec version name a) specDepParser)

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
findDep = lexeme (takeWhile1P (Just "dep") (\a -> not $ C.isSpace a))


findVersion :: Parser Text
findVersion = do
      _ <- char '('
      result <- lexeme (takeWhileP (Just "version") (\a -> a /= ')'))
      _ <- char ')'
      pure result


dependenciesSectionParser :: Parser Section
dependenciesSectionParser = L.nonIndented scn (L.indentBlock scn p)
                  where
                        p = do
                              _ <- chunk "DEPENDENCIES"
                              pure $ L.IndentMany Nothing (\a -> pure $ DependencySection a ) findDependency

findDependency :: Parser DirectDep
findDependency = do
      dep <- findDep
      _ <- ignored
      pure $ DirectDep dep