
module Strategy.Cocoapods.PodfileLock
  ( discover
  , strategy
  , analyze
  , configure
  , findSections

  , Dep (..)
  , Pod (..)
  , Section (..)
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

discover :: Discover
discover = Discover
  { discoverName = "podfilelock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  for_ files $ \f ->
    when (fileName f == "Podfile.lock") $ output (configure f)
  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "podfile-lock"
  , strategyAnalyze = \opts -> analyze & fileInputParser findSections (targetFile opts)
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts

analyze :: Member (Input [Section]) r => Sem r (Graphing Dependency)
analyze = buildGraph <$> input

data PodfilePkg = PodfilePkg { pkgName :: Text }
  deriving (Eq, Ord, Show, Generic)

type instance PkgLabel PodfilePkg = PodfileLabel

data PodfileLabel =
    PodfileVersion Text
  -- | PkgRemote Text
  deriving (Eq, Ord, Show, Generic)

toDependency :: PodfilePkg -> Set PodfileLabel -> Dependency
toDependency pkg = foldr applyLabel start
  where

  start :: Dependency
  start = Dependency
    { dependencyType = PodType
    , dependencyName = pkgName pkg
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyTags = M.empty
    }

  applyLabel :: PodfileLabel -> Dependency -> Dependency
  applyLabel (PodfileVersion ver) dep = dep { dependencyVersion = Just (CEq ver) }
  -- applyLabel (PkgRemote repo) dep = dep { dependencyLocations = repo : dependencyLocations dep }

buildGraph :: [Section] -> Graphing Dependency
buildGraph sections = run . withLabeling toDependency $
  traverse_ addSection sections
  where
  addSection :: Member (LabeledGrapher PodfilePkg) r => Section -> Sem r ()
  addSection (DependencySection deps) = traverse_ (direct . PodfilePkg . depName) deps
  addSection (PodSection pods) = traverse_ addSpec pods
  -- addSection (SpecRepos remotes) = traverse_ (addSpec (OtherRemote remote)) specs
  addSection _ = pure ()

  addSpec :: Member (LabeledGrapher PodfilePkg) r => Pod -> Sem r ()
  addSpec pod = do
    let pkg = PodfilePkg (name pod)
    -- add edges between spec and specdeps
    traverse_ (edge pkg . PodfilePkg . depName) (specs pod)
    -- add a label for version
    label pkg (PodfileVersion (version pod))

type Parser = Parsec Void Text

data Section =
      PodSection [Pod]
      | DependencySection [Dep]
      | UnknownSection Text
      | SpecRepos [Remote] 
      deriving (Eq, Ord, Show, Generic)

newtype Dep = Dep
      { depName :: Text
      } deriving (Eq, Ord, Show, Generic)

data Pod = Pod
     { name      :: Text
     , version   :: Text
     , specs     :: [Dep]
     } deriving (Eq, Ord, Show, Generic)

data Remote = Remote
     { location      :: Text
     , deps          :: [Dep]
     } deriving (Eq, Ord, Show, Generic)


findSections :: Parser [Section]
findSections = many (try podSectionParser <|> try dependenciesSectionParser <|> try specRepoParser <|> try emptySection) <* eof

emptySection :: Parser Section
emptySection = do 
      emptyLine <- restOfLine
      _ <- eol
      pure $ UnknownSection emptyLine

podSectionParser :: Parser Section
podSectionParser = L.nonIndented scn (L.indentBlock scn p)
      where
        p = do
          _ <- chunk "PODS:"
          return (L.IndentMany Nothing (\pods -> pure $ PodSection pods) podParser)

dependenciesSectionParser :: Parser Section
dependenciesSectionParser = L.nonIndented scn (L.indentBlock scn p)
      where
        p = do
          _ <- chunk "DEPENDENCIES:"
          return (L.IndentMany Nothing (\dependencies -> pure $ DependencySection dependencies) depParser)

specRepoParser :: Parser Section
specRepoParser = L.nonIndented scn (L.indentBlock scn p)
      where
        p = do
          _ <- chunk "SPEC REPOS:"
          return (L.IndentMany Nothing (\remotes -> pure $ SpecRepos remotes) remoteParser)

remoteParser :: Parser Remote
remoteParser = L.indentBlock scn p
    where 
      p = do
        location <- restOfLine
        pure (L.IndentMany Nothing (\deps -> pure $ Remote (T.dropWhileEnd (==':') location) deps) depParser)

podParser :: Parser Pod
podParser = L.indentBlock scn p
    where 
      p = do
        _ <- chunk ("- ")
        name <- findDep
        version <- findVersion
        _ <- restOfLine
        pure (L.IndentMany Nothing (\deps -> pure $ Pod name version deps) depParser)

depParser :: Parser Dep
depParser = do
      _ <- chunk ("- ")
      name <- findDep
      _ <- restOfLine
      pure $ Dep name

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
