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

{-
Target Filtering Workflow
1. If any legacy filters exist, apply them and skip the rest of the filtering logic.
2. Get targets that match the targets.only filters.
3. Get targets that match the paths.only filters.
4. Combine the results from step 2 & 3. If they are empty, then default to all targets allowed.
5. Remove the targets from step 4 match targets.exclude
6. Remove the targets from step 5 that match paths.exclude
-}
applyFiltersNew :: CombinedFilters -> Text -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
applyFiltersNew (CombinedFilters [] targetFilters pathFilters) tool dir targets = do
  let onlyTargetMatches = case targetsOnly <$> targetFilters of
        Just filters -> mapMaybe (\one -> applyTargetFilter one tool dir targets True) filters
        Nothing -> []
        
  let onlyPathMatches = case pathsOnly <$> pathFilters of
        Just filters -> mapMaybe (\one -> applyPathFilter one dir targets True) filters
        Nothing -> []

  let allOnlyMatches = if null $ onlyPathMatches <> onlyTargetMatches
                       then [targets]
                       else onlyPathMatches <> onlyTargetMatches

  onlyMatches <- NE.nonEmpty allOnlyMatches
  let onlyFilteredTargets = sconcat onlyMatches
  excludeTargetMatches <- NE.nonEmpty $ case targetsExclude <$> targetFilters of
        Nothing -> [onlyFilteredTargets]
        Just [] -> [onlyFilteredTargets]
        Just filters -> mapMaybe (\one -> applyTargetFilter one tool dir onlyFilteredTargets False) filters

  let remainingTargets = sconcat excludeTargetMatches
  excludePathMatches <- NE.nonEmpty $ case pathsExclude <$> pathFilters of
        Nothing -> [remainingTargets]
        Just [] -> [remainingTargets]
        Just filters -> mapMaybe (\one -> applyPathFilter one dir remainingTargets False) filters

  pure (sconcat excludePathMatches)
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