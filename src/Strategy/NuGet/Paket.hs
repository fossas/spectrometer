module Strategy.NuGet.Paket
  ( discover
  , strategy
  , analyze
  , configure
  , findSections

  , PaketDep(..)
  , Section(..)
  , Remote(..)
--   , Spec(..)
--   , SpecDep(..)
--   , DirectDep(..)
--   , Section(..)
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
import           Graphing (Graphing)
import           Types
import           Text.Megaparsec hiding (label)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

discover :: Discover
discover = Discover
  { discoverName = "paket"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  for_ files $ \f ->
    when (fileName f == "paket.lock") $ output (configure f)
  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "paket"
  , strategyAnalyze = \opts -> analyze & fileInputParser findSections (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts

analyze :: Member (Input [Section]) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

data PaketPkg = PaketPkg { pkgName :: Text }
  deriving (Eq, Ord, Show, Generic)

type instance PkgLabel PaketPkg = PaketLabel

data PaketLabel =
    PaketVersion Text
  | PaketRemote Text
  | GroupName Text
  deriving (Eq, Ord, Show, Generic)

toDependency :: PaketPkg -> Set PaketLabel -> Dependency
toDependency pkg = foldr applyLabel start
  where

  start :: Dependency
  start = Dependency
    { dependencyType = NuGetType
    , dependencyName = pkgName pkg
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyTags = M.empty
    }

  applyLabel :: PaketLabel -> Dependency -> Dependency
  applyLabel (PaketVersion ver) dep = dep { dependencyVersion = Just (CEq ver) }
  applyLabel (GroupName name) dep = dep { dependencyTags = M.insertWith (++) "group" [name] (dependencyTags dep) }
  applyLabel (PaketRemote repo) dep = dep { dependencyLocations = repo : dependencyLocations dep }
  
buildGraph :: [Section] -> Graphing Dependency
buildGraph sections = run . withLabeling toDependency $
      traverse_ (addSection "MAIN") sections
      where
            addSection :: Member (LabeledGrapher PaketPkg) r => Text -> Section -> Sem r ()
            addSection group (GitSection remotes) = traverse_ (addRemote group) remotes
            addSection group (NugetSection remotes) = traverse_ (addRemote group) remotes
            addSection group (HTTPSection remotes) =  traverse_ (addRemote group) remotes
            addSection _ (GroupSection group gSections) = traverse_ (addSection group) gSections
            addSection _ (UnknownSection _) = pure ()

            addRemote :: Member (LabeledGrapher PaketPkg) r => Text -> Remote -> Sem r ()
            addRemote group remote = traverse_ (addSpec (PaketRemote $ location remote) (GroupName group)) (dependencies remote) 

            addSpec :: Member (LabeledGrapher PaketPkg) r => PaketLabel -> PaketLabel -> PaketDep -> Sem r ()
            addSpec remote group dep = do
              let pkg = PaketPkg (name dep)
              -- add edges between spec and specdeps
              traverse_ (edge pkg . PaketPkg) (transitive dep)
              -- add a label for version
              label pkg (PaketVersion (version dep))
              -- add a label for remote and group
              label pkg remote
              label pkg group

              direct pkg

type Name = Text

data Section =
      GitSection [Remote]
      | NugetSection [Remote]
      | HTTPSection [Remote]
      | UnknownSection Text
      | GroupSection Name [Section]
      deriving (Eq, Ord, Show, Generic)

data Remote = Remote
     { location      :: Text
     , dependencies  :: [PaketDep]
     } deriving (Eq, Ord, Show, Generic)

data PaketDep = PaketDep
     { name       :: Text
     , version    :: Text
     , transitive :: [Text]
     } deriving (Eq, Ord, Show, Generic)

data Group = Group
     { groupName    :: Text
     , groupSections :: [Section]
     } deriving (Eq, Ord, Show, Generic)


findSections :: Parser [Section]
findSections = many (try githubSectionParser <|> try httpSectionParser <|> try nugetSectionParser <|> try groupSection <|> try emptySection) <* eof

groupSection :: Parser Section
groupSection = do
          _ <- chunk "GROUP"
          name <- textValue
          sections <- many (try githubSectionParser <|> try httpSectionParser <|> try nugetSectionParser <|>  try emptySection)
          pure $ GroupSection name sections

emptySection :: Parser Section
emptySection = do 
      emptyLine <- restOfLine
      guard $ not $ "GROUP" `T.isPrefixOf` emptyLine
      _ <- eol
      pure $ UnknownSection emptyLine

httpSectionParser :: Parser Section
httpSectionParser = L.nonIndented scn (L.indentBlock scn p)
      where
        p = do
          _ <- chunk "HTTP"
          return (L.IndentMany Nothing (\remotes -> pure $ HTTPSection remotes) remoteParser)

githubSectionParser :: Parser Section
githubSectionParser = L.nonIndented scn (L.indentBlock scn p)
      where
        p = do
          _ <- chunk "GITHUB"
          return (L.IndentMany Nothing (\remotes -> pure $ GitSection remotes) remoteParser)

nugetSectionParser :: Parser Section
nugetSectionParser = L.nonIndented scn (L.indentBlock scn p)
      where
        p = do
          _ <- chunk "NUGET"
          return (L.IndentMany Nothing (\remotes -> pure $ NugetSection remotes) remoteParser)

-- fieldName,  (value, deps)
remoteParser :: Parser Remote
remoteParser = L.indentBlock scn p
  where
    p = do
      _ <- chunk "remote:"
      value <- textValue
      return (L.IndentMany Nothing (\specs -> pure $ Remote value specs) specParser)

textValue :: Parser Text
textValue = do
      _ <- chunk " "
      restOfLine

specParser :: Parser PaketDep
specParser = L.indentBlock scn p
    where 
      p = do
        name <- findDep
        version <- findVersion
        _ <- restOfLine
        pure (L.IndentMany Nothing (\a -> pure $ PaketDep name version a) specsParser)
      
specsParser :: Parser Text
specsParser = do
      name <- findDep
      _ <- restOfLine
      pure name

findDep :: Parser Text
findDep = lexeme (takeWhile1P (Just "dep") (not . C.isSpace))

findVersion :: Parser Text
findVersion = do
      _ <- char '('
      result <- lexeme (takeWhileP (Just "version") (/= ')'))
      _ <- char ')'
      pure result

restOfLine :: Parser Text
restOfLine = takeWhileP (Just "ignored") (not . isEndLine)

isEndLine :: Char -> Bool
isEndLine '\n' = True
isEndLine '\r' = True
isEndLine _    = False

scn :: Parser ()
scn =  L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc =  L.space (void $ some (char ' ')) empty empty
