{-# LANGUAGE TemplateHaskell #-}

module Discovery.Filters (
  BuildTargetFilterOld (..),
  CombinedFilters (..),
  filterParser,
  applyFilter,
  applyFiltersOld,
  applyFilters,
) where

import App.Fossa.Configuration
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup (sconcat)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Path (Dir, Path, Rel, isProperPrefixOf, parseRelDir)
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, try),
  Parsec,
  satisfy,
  some,
  (<|>),
 )
import Text.Megaparsec.Char
import Types (BuildTarget (..))

data CombinedFilters = CombinedFilters
  { legacyFilters :: [BuildTargetFilterOld]
  , targetFilters :: Maybe ConfigTargets
  , pathFilters :: Maybe ConfigPaths
  }

data BuildTargetFilterOld
  = -- | buildtool, directory. if a project matches this filter, all of its
    -- build targets will be analyzed
    ProjectFilter Text (Path Rel Dir)
  | -- | buildtool, directory, build target. if a target matches this filter,
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
applyFilters :: CombinedFilters -> Text -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
applyFilters (CombinedFilters [] Nothing Nothing) _ _ targets = Just targets
applyFilters (CombinedFilters [] targetFilters pathFilters) tool dir targets = do
  let onlyTargetMatches = do
        filters <- targetsOnly <$> targetFilters
        _ <- NE.nonEmpty filters
        Just $ mapMaybe (\one -> applyTargetFilter one tool dir targets) filters

  let onlyPathMatches = do
        filters <- pathsOnly <$> pathFilters
        _ <- NE.nonEmpty filters
        Just $ mapMaybe (\one -> applyPathFilter one dir targets) filters

  matchesOnly <- validateOnlyTargets onlyTargetMatches onlyPathMatches targets
  allOnlyMatches <- checkEmptySet $ foldl S.union S.empty matchesOnly

  let excludeTargetMatches = fromMaybe allOnlyMatches $ do
        filters <- targetsExclude <$> targetFilters
        let remove = mapMaybe (\one -> applyTargetFilter one tool dir allOnlyMatches) filters
        pure $ S.difference allOnlyMatches $ foldl S.union S.empty remove

  _ <- checkEmptySet excludeTargetMatches
  let finalTargets = fromMaybe excludeTargetMatches $ do
        filters <- pathsExclude <$> pathFilters
        let remove = mapMaybe (\one -> applyPathFilter one dir excludeTargetMatches) filters
        pure $ S.difference excludeTargetMatches $ foldl S.union S.empty remove

  checkEmptySet finalTargets
  where
    checkEmptySet :: Set BuildTarget -> Maybe (Set BuildTarget)
    checkEmptySet set = if S.null set then Nothing else Just set

    -- If both are Nothing, then we can keep the default list of targets.
    -- If even one is an array, we need to validate that it has elements.
    validateOnlyTargets :: Maybe [Set BuildTarget] -> Maybe [Set BuildTarget] -> Set BuildTarget -> Maybe [Set BuildTarget]
    validateOnlyTargets Nothing Nothing base = Just [base]
    validateOnlyTargets (Just first) Nothing _ = checkEmptyList first
    validateOnlyTargets Nothing (Just second) _ = checkEmptyList second
    validateOnlyTargets (Just first) (Just second) _ = checkEmptyList $ first <> second

    checkEmptyList :: [a] -> Maybe [a]
    checkEmptyList [] = Nothing
    checkEmptyList l = Just l
applyFilters (CombinedFilters legacyFilters _ _) tool dir targets = applyFiltersOld legacyFilters tool dir targets

-- If include is set and the tool type matches, return all targets.
-- If exclude is set and nothing matches, return all targets.
applyTargetFilter :: ConfigTarget -> Text -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
applyTargetFilter (ConfigTarget configType Nothing) tool _ targets
  | configType == tool = Just targets
  | otherwise = Nothing
applyTargetFilter (ConfigTarget configType (Just (DirectoryFilter dir))) tool dir' targets
  | configType == tool && dir == dir' = Just targets
  | otherwise = Nothing
applyTargetFilter (ConfigTarget configType (Just (ExactTargetFilter dir target))) tool dir' targets
  | configType == tool && dir == dir' && S.member (BuildTarget target) targets = Just $ S.singleton (BuildTarget target)
  | otherwise = Nothing

applyPathFilter :: Path Rel Dir -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
applyPathFilter pathFilter dir targets
  | (pathFilter `isProperPrefixOf` dir) || (pathFilter == dir) = Just targets
  | otherwise = Nothing

-- | Apply a set of filters determining:
-- 1. Whether the project should be scanned (@Maybe@)
-- 2. The build targets that should be scanned (@Set BuildTarget@)
--
-- This is the same as using 'applyFilter' on each filter in the list, unioning
-- the successful results. If all filters fail, this returns @Nothing@
applyFiltersOld :: [BuildTargetFilterOld] -> Text -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
applyFiltersOld [] _ _ targets = Just targets
applyFiltersOld filters tool dir targets = do
  let individualResults = mapMaybe (\one -> applyFilter one tool dir targets) filters
  successful <- NE.nonEmpty individualResults
  pure (sconcat successful)

-- | Apply a filter to a set of BuildTargets, returning:
-- 1. Whether the targets should be scanned (@Maybe@)
-- 2. The BuildTargets that should be scanned (@Set BuildTarget@)
applyFilter :: BuildTargetFilterOld -> Text -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
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

filterParser :: Parser BuildTargetFilterOld
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