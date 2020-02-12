module Strategy.Go.GopkgLock
  ( discover
  , analyze

  , Project(..)

  , buildGraph
  )
  where

import Prologue hiding ((.=))

import Polysemy
import Polysemy.Error
import Polysemy.Output
import Toml (TomlCodec, (.=))
import qualified Toml

import DepTypes
import Diagnostics
import Discovery.Walk
import Effect.Exec
import Effect.LabeledGrapher
import Effect.ReadFS
import Graphing (Graphing)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import Types

discover :: Discover
discover = Discover
  { discoverName = "gopkglock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Exec, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "Gopkg.lock") files of
    Nothing -> pure ()
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ (output . dummyConfigure "golang-gopkglock" Optimal NotComplete (parent file)) res

  walkContinue

golockCodec :: TomlCodec GoLock
golockCodec = GoLock
  <$> Toml.list projectCodec "projects" .= lockProjects

projectCodec :: TomlCodec Project
projectCodec = Project
  <$> Toml.text "name" .= projectName
  <*> Toml.dioptional (Toml.text "source") .= projectSource
  <*> Toml.text "revision" .= projectRevision

newtype GoLock = GoLock
  { lockProjects :: [Project]
  } deriving (Eq, Ord, Show, Generic)

data Project = Project
  { projectName     :: Text
  , projectSource   :: Maybe Text
  , projectRevision :: Text
  } deriving (Eq, Ord, Show, Generic)

analyze :: Members '[Exec, ReadFS, Error ReadFSErr] r => Path Rel File -> Sem r (Graphing Dependency)
analyze file = graphingGolang $ do
  contents <- readContentsText file
  case Toml.decode golockCodec contents of
    Left err -> throw (FileParseError (fromRelFile file) (Toml.prettyException err))
    Right golock -> do
      buildGraph (lockProjects golock)

      -- TODO: diagnostics?
      _ <- runError @ExecErr (fillInTransitive (parent file))
      pure ()

buildGraph :: Member (LabeledGrapher GolangPackage) r => [Project] -> Sem r ()
buildGraph = void . traverse_ go
  where
  go :: Member (LabeledGrapher GolangPackage) r => Project -> Sem r ()
  go Project{..} = do
    let pkg = mkGolangPackage projectName

    direct pkg
    label pkg (mkGolangVersion projectRevision)

    -- label location when it exists
    traverse_ (label pkg . GolangLabelLocation) projectSource
