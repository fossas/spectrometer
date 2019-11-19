module Strategy.Go.Transitive
  ( fillInTransitive
  , fillInTransitive'
  )
  where

import Prologue hiding (empty)

import           Control.Applicative (many)
import qualified Data.Attoparsec.ByteString as A
import           Data.Aeson.Internal (iparse)
import           Data.Aeson.Parser
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import           Polysemy
import           Polysemy.Error
import           Polysemy.State

import           Diagnostics
import           Effect.Exec
import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Go.Types

goListCmd :: Command
goListCmd = Command
  { cmdNames = ["go"]
  , cmdBaseArgs = ["list", "-json", "all"]
  , cmdAllowErr = NonEmptyStdout
  }

data Package = Package
  { packageImportPath :: Text
  , packageModule     :: Maybe Module
  , packageImports    :: Maybe [Text]
  , packageSystem     :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

data Module = Module
  { modPath    :: Text
  , modVersion :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \obj ->
    Package <$> obj .:  "ImportPath"
            <*> obj .:? "Module"
            <*> obj .:? "Imports"
            <*> obj .:? "Standard"

instance FromJSON Module where
  parseJSON = withObject "Module" $ \obj ->
    Module <$> obj .:  "Path"
           <*> obj .:? "Version"

-- `go list -json` is dumb: it outputs a bunch of raw json objects:
--     {
--       ...
--     }
--     {
--       ...
--     }
-- decodeMany is our workaround. it produces `[a]` by repeatedly parsing
-- json objects, wrapping them into `[Value]`, then decoding `[Value]`
-- into `[a]`
decodeMany :: FromJSON a => BL.ByteString -> Maybe [a]
--decodeMany = decodeWith parser fromJSON
decodeMany input = case eitherDecodeWith parser (iparse parseJSON) input of
    Left err -> traceShow err Nothing
    Right a -> pure a
  where
  -- skipSpace is lifted from Data.Aeson.Parser.Internal
  skipSpace = A.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

  parser = do
    (objects :: [Value]) <- many json <* skipSpace <* A.endOfInput
    pure (Array (V.fromList objects))


doItTransitive :: Member (Graphing GolangPackage) r => Map Text Package -> Sem r ()
doItTransitive = void . M.traverseWithKey go
  where
  go :: Member (Graphing GolangPackage) r => Text -> Package -> Sem r ()
  go name package = unless (packageSystem package == Just True) $ do
    let -- when a gomod field is present, use that for the package import path
        -- otherwise use the top-level package import path
        name :: Text
        name = fromMaybe (packageImportPath package) (modPath <$> packageModule package)

        pkg :: GolangPackage
        pkg = GolangPackage name

    traverse_ (traverse_ (\dep -> edgeg pkg (GolangPackage dep))) (packageImports package)

    -- when we have a gomod, and that gomod has a version, add label for version
    case modVersion =<< packageModule package of
      Nothing -> pure ()
      Just ver -> labelg pkg (GolangVersion (fixVersion ver))


fillInTransitive' :: Members '[Error ExecErr, Exec, Graphing GolangPackage] r => Path Rel Dir -> Sem r ()
fillInTransitive' dir = do
  goListOutput <- execThrow dir goListCmd []
  case decodeMany goListOutput of
    Nothing -> throw (CommandParseError "" "couldn't parse output of `go list -json all`") -- TODO: command name??
    -- TODO: packageImportPath might be different from modPath. does this matter?
    Just (packages :: [Package]) -> doItTransitive $ indexBy packageImportPath packages


fillInTransitive :: Members '[Error ExecErr, Exec] r => Map Text G.DepRef -> Path Rel Dir -> G.Graph -> Sem r G.Graph
fillInTransitive depMap dir graph = evalGraphBuilder graph $ runState depMap $ do -- TODO: void????
  goListOutput <- execThrow dir goListCmd []
  case decodeMany goListOutput of
    Nothing -> throw (CommandParseError "" "couldn't parse output of `go list -json all`") -- TODO: command name??
    Just (packages :: [Package]) -> M.traverseWithKey (fillInSingle (indexBy packageImportPath packages)) =<< get

fillInSingle :: forall r. Members '[GraphBuilder, State (Map Text G.DepRef)] r => Map Text Package -> Text -> G.DepRef -> Sem r ()
fillInSingle packages name parentRef = do
  for_ (M.lookup name packages) $
    \(Package _ _ maybeImports system) -> unless (system == Just True) $
      for_ maybeImports $ \imports ->
        for_ imports $ \imported -> do
          maybeChildRef <- addDep imported
          case maybeChildRef of
            Nothing -> pure ()
            Just childRef -> do
              fillInSingle packages imported childRef
              addEdge parentRef childRef

  where
  addDep :: Text -> Sem r (Maybe G.DepRef)
  addDep path = do
    maybeRef <- M.lookup path <$> get @(Map Text G.DepRef)
    case maybeRef of
      Just ref -> pure (Just ref)
      Nothing -> do
        case M.lookup path packages of
          Nothing -> pure Nothing
          Just package ->
            if (packageSystem package == Just True)
              then pure Nothing
              else do
                ref <- addNode (toDependency package)
                modify (M.insert path ref)
                pure (Just ref)

toDependency :: Package -> G.Dependency
toDependency package =
  case packageModule package of
    Nothing -> G.Dependency
      { dependencyType = G.GoType
      , dependencyName = packageImportPath package
      , dependencyVersion = Nothing
      , dependencyLocations = []
      , dependencyTags = M.empty
      }
    Just gomod -> G.Dependency
      { dependencyType = G.GoType
      , dependencyName = modPath gomod
      , dependencyVersion = (G.CEq . last . T.splitOn "-" . T.replace "+incompatible" "") <$> modVersion gomod
      , dependencyLocations = []
      , dependencyTags = M.empty
      }

indexBy :: Ord k => (v -> k) -> [v] -> Map k v
indexBy key = M.fromList . map (\v -> (key v, v))
