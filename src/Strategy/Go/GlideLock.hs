module Strategy.Go.GlideLock
  ( discover
  , analyze

  , GlideLockfile(..)
  , GlideDep(..)

  , buildGraph
  )
  where

import Prologue hiding ((.=))

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Polysemy
import Polysemy.Error
import Polysemy.Output

import DepTypes
import Diagnostics
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Types

discover :: Discover
discover = Discover
  { discoverName = "glidelock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "glide.lock") files of
    Nothing -> pure ()
    Just file  -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ (output . dummyConfigure "golang-glidelock" Optimal NotComplete (parent file)) res

  walkContinue

analyze :: Members '[ReadFS, Error ReadFSErr] r => Path Rel File -> Sem r (Graphing Dependency)
analyze file = do
  lockfile <- readContentsYaml @GlideLockfile file
  pure (buildGraph lockfile)

buildGraph :: GlideLockfile -> Graphing Dependency
buildGraph lockfile = unfold direct (const []) toDependency
  where
  direct = imports lockfile
  toDependency GlideDep{..}  =
    Dependency { dependencyType = GoType
               , dependencyName = depName
               , dependencyVersion = Just (CEq $ T.pack (show depVersion))
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

data GlideLockfile = GlideLockfile
  { hash    :: Integer
  , updated :: Text
  , imports :: [GlideDep]
  } deriving (Eq, Ord, Show, Generic)

data GlideDep = GlideDep
  { depName    :: Text
  , depVersion :: Integer
  , depRepo    :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON GlideLockfile where
  parseJSON = withObject "GlideLockfile" $ \obj ->
    GlideLockfile <$> obj .: "hash"
                  <*> obj .: "updated"
                  <*> obj .: "imports"

instance FromJSON GlideDep where
  parseJSON = withObject "GlideDep" $ \obj ->
    GlideDep <$> obj .:  "name"
               <*> obj .: "version"
               <*> obj .:? "repo"
