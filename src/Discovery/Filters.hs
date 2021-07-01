module Discovery.Filters (
  BuildTargetFilter (..),
  CombinedFilters (..),
  filterParser,
  applyFilter,
  applyFilters,
  applyFiltersNew,
) where

import App.Fossa.Configuration
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Path
import Text.Megaparsec
import Text.Megaparsec.Char
import Types (BuildTarget (..))

data CombinedFilters = CombinedFilters
  { legacyFilters :: [BuildTargetFilter]
  , targetFilters :: Maybe ConfigTargets
  , pathFilters :: Maybe ConfigPaths
  }

data BuildTargetFilter
  = -- | buildtool, directory. if a project matches this filter, all of its
    -- buildtargets will be analyzed
    ProjectFilter Text (Path Rel Dir)
  | -- | buildtool, directory, buildtarget. if a target matches this filter,
    -- only that target will be analyzed
    TargetFilter Text (Path Rel Dir) BuildTarget
  deriving (Eq, Ord, Show)

applyFiltersNew :: CombinedFilters -> Text -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
applyFiltersNew (CombinedFilters [] targetFilters pathFilters) tool dir targets = do
  targetFilteredTargets <- NE.nonEmpty $ case targetFilters of
    Just filters -> do
      onlyFiltered <- case targetsOnly filters of
        [] -> pure targets
        onlyTargets -> mapMaybe (\one -> applyTargetFilter one tool dir targets True) onlyTargets
      case targetsExclude filters of
        [] -> pure onlyFiltered
        excludeTargets -> mapMaybe (\one -> applyTargetFilter one tool dir onlyFiltered False) excludeTargets
    Nothing -> [targets]

  filteredTargets <- NE.nonEmpty $ case pathFilters of
    Just filters -> do
      onlyFiltered <- case pathsOnly filters of
        [] -> pure targets
        onlyPaths -> mapMaybe (\one -> applyPathFilter one dir (sconcat targetFilteredTargets) True) onlyPaths
      case pathsExclude filters of
        [] -> pure targets
        excludePaths -> mapMaybe (\one -> applyPathFilter one dir onlyFiltered False) excludePaths
    Nothing -> [targets]

  pure (sconcat filteredTargets)
applyFiltersNew (CombinedFilters legacyFilters _ _) tool dir targets = applyFilters legacyFilters tool dir targets

-- Logical xand that handles include and exclude logic.
-- If include is set and the tool type matches, return all targets.
-- If exclude is set and nothing matches, return all targets.
applyTargetFilter :: ConfigTarget -> Text -> Path Rel Dir -> Set BuildTarget -> Bool -> Maybe (Set BuildTarget)
applyTargetFilter (ConfigTarget configType Nothing) tool _ targets onlyTargets
  | configType == tool && onlyTargets = Just targets
  | otherwise = Nothing
applyTargetFilter (ConfigTarget configType (Just (DirectoryFilter dir))) tool dir' targets onlyTargets
  | configType == tool && dir == dir' && onlyTargets = Just targets
  | otherwise = Nothing
applyTargetFilter (ConfigTarget configType (Just (ExactTargetFilter dir target))) tool dir' targets onlyTargets
  | configType == tool && dir == dir' && S.member (BuildTarget target) targets && onlyTargets = Just $ S.singleton (BuildTarget target)
  | otherwise = Nothing

applyPathFilter :: Path Rel Dir -> Path Rel Dir -> Set BuildTarget -> Bool -> Maybe (Set BuildTarget)
applyPathFilter pathFilter dir targets onlyPaths
  | dir == pathFilter && onlyPaths = Just targets
  | otherwise = Nothing

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
  successful <- NE.nonEmpty individualResults
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
