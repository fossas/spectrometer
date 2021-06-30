module Discovery.Filters (
  BuildTargetFilter (..),
  filterParser,
  applyFilter,
  applyFilters,
  applyFiltersNew,
) where

import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import App.Fossa.Configuration
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Path
import Text.Megaparsec
import Text.Megaparsec.Char
import Types (BuildTarget (..))
import Debug.Trace
import App.Fossa.Configuration
import Data.Maybe (catMaybes)

data BuildTargetFilter
  = -- | buildtool, directory. if a project matches this filter, all of its
    -- buildtargets will be analyzed
    ProjectFilter Text (Path Rel Dir)
  | -- | buildtool, directory, buildtarget. if a target matches this filter,
    -- only that target will be analyzed
    TargetFilter Text (Path Rel Dir) BuildTarget
  -- | 
  --   PathFilter Text
  -- | 
  --   NewTargetFilter Text Text
  deriving (Eq, Ord, Show)


{- New filters workflow with paths and targets

1. Get the target
2. If "only" exists, check if it matches the list of only targets or paths
3. If "exclude" exists, check if it matches exclude list of only targets or paths and remove it
4. Return the target


Implementation
- Parse a list of filters
- Convert them to internal filter
  - File path
  - Target type
- Iterate over the only list
- Iterate over the exclude list
- Return targets

Don't follow what we already have, rip it out and start over.
-}


applyFiltersNew :: Maybe ConfigFile -> Text -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
applyFiltersNew Nothing _ _ targets = Just targets
applyFiltersNew filters tool dir targets = do
  _ <- traceM "targets"
  _ <- traceM $ show targets
  let individualResults = case filters >>= configTargets of
                            Just targetFilters -> do
                              onlyFiltered <- case targetsOnly targetFilters of
                                                [] -> pure targets
                                                onlyTargets -> mapMaybe (\one -> applyTargetFilter one tool targets True) onlyTargets
                              case targetsExclude targetFilters of
                                                [] -> pure onlyFiltered
                                                excludeTargets -> mapMaybe (\one -> applyTargetFilter one tool targets True) excludeTargets
                            Nothing -> [targets]
  _ <- traceM "results"
  _ <- traceM $ show individualResults
  setResults <- NE.nonEmpty individualResults
  _ <- traceM "non empty res"
  _ <- traceM $ show setResults
  -- let finalResults = case filters >>= configPaths of
  --                           Just pathFilters -> do
  --                             list <- mapMaybe (\one -> applyPathFilter one dir targets True) (pathsOnly pathFilters)
  --                             _ <- traceM $ show list
  --                             mapMaybe (\one -> applyPathFilter one dir list False) (pathsExclude pathFilters)
  --                           Nothing -> [targets]
  -- successful <- NE.nonEmpty finalResults

  pure (sconcat setResults)


applyTargetFilter :: ConfigTarget -> Text -> Set BuildTarget -> Bool -> Maybe (Set BuildTarget)
applyTargetFilter (ConfigTarget configType Nothing) tool targets onlyTargets
  -- Logical xand that handles include and exclude logic
  | configType == tool && onlyTargets = Just targets | otherwise = Nothing

  -- Build out support for this use case 
applyTargetFilter (ConfigTarget _ _) _ targets _ = Just targets

-- need to handle bullshit like `./path` and `path` and `path/`
-- Add tests for this
applyPathFilter :: Text -> Path Rel Dir -> Set BuildTarget -> Bool -> Maybe (Set BuildTarget)
applyPathFilter filter dir targets onlyPaths = do
  _ <- traceM $ show dir 
  _ <- traceM $ show filter 
  Just targets


-- | Apply a set of filters determining:
-- 1. Whether the project should be scanned (@Maybe@)
-- 2. The buildtargets that should be scanned (@Set BuildTarget@)
--
-- This is the same as using 'applyFilter' on each filter in the list, unioning
-- the successful results. If all filters fail, this returns @Nothing@
applyFilters :: [BuildTargetFilter] -> Text -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
applyFilters [] _ _ targets = Just targets
applyFilters filters tool dir targets = do
  let individualResults = mapMaybe (\one -> applyFilter one tool dir targets) filters
  
  traceM $ show individualResults
  successful <- NE.nonEmpty individualResults
  traceM $ show successful

  pure (sconcat successful)

-- | Apply a filter to a set of BuildTargets, returning:
-- 1. Whether the targets should be scanned (@Maybe@)
-- 2. The BuildTargets that should be scanned (@Set BuildTarget@)
applyFilter :: BuildTargetFilter -> Text -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
applyFilter (ProjectFilter tool dir) tool' dir' targets
  | tool == tool'
    , dir == dir' =
    Just targets
  | otherwise = Nothing
applyFilter (TargetFilter tool dir target) tool' dir' targets
  | tool == tool'
    , dir == dir'
    , S.member target targets =
    Just $ S.singleton target
  | otherwise = Nothing

type Parser = Parsec Void Text

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

    target :: Parser BuildTarget
    target = BuildTarget <$> takeWhile1P Nothing (const True)
