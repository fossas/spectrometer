{-# LANGUAGE TemplateHaskell #-}

module Discovery.Filters (
  BuildTargetFilterOld (..),
  CombinedFilters (..),
  filterParser,
  applyFilter,
  applyFiltersOld,
  applyFilters,
  TargetTest (..),
  Comb (..),
  Determination (..),
  apply,
) where

import App.Fossa.Configuration
import Data.List (intersect, (\\))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup (sconcat)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Path
import Path (Dir, Path, Rel, isProperPrefixOf, parseRelDir)
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, try),
  Parsec,
  satisfy,
  some,
  (<|>),
 )
import Text.Megaparsec.Char
import Types (BuildTarget (..), FoundTargets (..))

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

-- two filters:
-- path-only: dir
-- buildtarget-only: mvn@foo
--
-- project: mvn@foo
--
--
-- concerns:
-- 1. what about analyzers that always produce an empty set of build targets?
-- 2. flattening the TargetFilter type (TargetTest)
-- 3. in an ideal world, what would our filter type be?
-- 4. how do path filters interact with buildtarget filters?
-- 5. legacy filters, when present, are the only ones that matter? why?
-- 6. warning that config file filters are getting ignored
--
-- future work to ticket:
-- - skip directories if filters will never match
-- - skip analyzers if filters will never match
--
-- buildtarget-paths and paths-filters get unioned
--
-- to look at:
-- - command line parser for include/excludes
--
-- config file, bunch of filters
-- --filter mvn@.
--
-- getting determination values from the target-only, path-only, target-exclude, path-exclude

data TargetTest = TypeTarget Text | TypeDirTarget Text (Path Rel Dir) | TypeDirTargetTarget Text (Path Rel Dir) BuildTarget

data Comb = Comb
  { combTargets :: [TargetTest]
  , combPaths :: [Path Rel Dir]
  }

combIsEmpty :: Comb -> Bool
combIsEmpty (Comb [] []) = True
combIsEmpty _ = False

-- (buildtargetfilters-only `union` pathfilters-only)
--   `subtract` (buildtargetfilters-exclude `union` pathfilters-exclude)
apply :: Comb -> Comb -> Text -> Path Rel Dir -> FoundTargets -> Determination
apply include exclude buildtool dir targets =
  dSubtract
    targets
    (if not (combIsEmpty include) then applyComb include buildtool dir else MatchAll)
    (applyComb exclude buildtool dir)

applyComb :: Comb -> Text -> Path Rel Dir -> Determination
applyComb comb buildtool dir =
  foldMap (\t -> applyTarget t buildtool dir) (combTargets comb)
    <> foldMap (`applyPath` dir) (combPaths comb)

-- mvn@foo:bar

-- mvn@foo ProjectWithoutTargets -> MatchNone
--
-- mvn@foo (FoundTargets xs) -> bar `elem` xs -> MatchSome [bar] otherwise MatchNone
applyTarget :: TargetTest -> Text -> Path Rel Dir -> Determination
applyTarget (TypeTarget t) u _ = if t == u then MatchAll else MatchNone
applyTarget (TypeDirTarget t p) u q = if t == u && p == q then MatchAll else MatchNone
applyTarget (TypeDirTargetTarget t p target) u q = if t == u && p == q then MatchSome (target NE.:| []) else MatchNone

{-
applyTarget TypeDirTargetTarget{} _ _ ProjectWithoutTargets = MatchNone
applyTarget (TypeDirTargetTarget t p target) u q (FoundTargets targets) =
  if t == u && p == q && S.member target targets
    then MatchSome (target NE.:| [])
    else MatchNone
-}

-- TODO: argument order docs
applyPath :: Path Rel Dir -> Path Rel Dir -> Determination
applyPath t u = if isProperPrefixOf t u || t == u then MatchAll else MatchNone

-- MatchNone <> MatchAll = MatchAll is the reason for this order
instance Semigroup Determination where
  MatchNone <> t = t
  t <> MatchNone = t
  t <> MatchAll = t -- TODO: contentious: should MatchAll override MatchSome?
  MatchAll <> t = t
  MatchSome ts <> MatchSome us = MatchSome (ts <> us)

-- only-target: mvn@foo:bar
-- only-target: mvn@foo
-- only-path: foo
-- exclude-target: mvn@foo:baz
--
-- my-project: mvn@foo ["bar","baz",15 other targets]
--
-- -> MatchAll?
-- -> MatchSome ["bar"]?

-- only-path: bar
-- only-target: mvn@bar/foo:baz
--
-- my-project: mvn@bar ["quux"] -> MatchAll
-- my-project: mvn@bar/foo ["baz", "bar", 15 others] -> MatchSome baz
-- my-project: mvn@bar/baz ["baz", "bar", 15 others] -> MatchAll
-- my-project: setuptools@bar/foo [] -> MatchAll

instance Monoid Determination where
  mempty = MatchNone

res =
  applyTarget
    (TypeDirTargetTarget "mvn" $(mkRelDir "foo") (BuildTarget "baz"))
    "mvn"
    $(mkRelDir "foo")
res2 =
  applyTarget
    (TypeDirTargetTarget "mvn" $(mkRelDir "foo") (BuildTarget "bar"))
    "mvn"
    $(mkRelDir "foo")
res3 =
  applyTarget
    (TypeTarget "mvn")
    "mvn"
    $(mkRelDir "foo")

-- newtype BuildTarget = BuildTarget String
-- TODO: NonEmptySet?
data Determination = MatchNone | MatchAll | MatchSome (NE.NonEmpty BuildTarget)
  deriving (Eq, Ord, Show)

-- TODO: there are some cases that we'd like to produce warnings
-- TODO: describe argument order; determination "include" and determination "exclude"
dSubtract :: FoundTargets -> Determination -> Determination -> Determination
dSubtract _ _ MatchAll = MatchNone
dSubtract _ MatchNone _ = MatchNone
dSubtract _ MatchAll MatchNone = MatchAll
dSubtract ProjectWithoutTargets MatchAll (MatchSome _) = MatchNone -- TODO: warn user about invalid filter
dSubtract (FoundTargets targets) MatchAll (MatchSome xs) =
  case NE.nonEmpty (S.toList targets \\ NE.toList xs) of
    Nothing -> MatchNone
    Just zs -> MatchSome zs
dSubtract _ (MatchSome xs) MatchNone = MatchSome xs
-- TODO: some targets might not be valid
dSubtract _ (MatchSome xs) (MatchSome ys) =
  case NE.nonEmpty (NE.toList xs \\ NE.toList ys) of
    Nothing -> MatchNone
    Just zs -> MatchSome zs

-- analyzer: returns empty set of build targets

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
  let onlyTargetMatches :: Maybe [Set BuildTarget]
      onlyTargetMatches = do
        filters <- targetsOnly <$> targetFilters
        _ <- NE.nonEmpty filters
        Just $ mapMaybe (\one -> applyTargetFilter one tool dir targets) filters

  let onlyPathMatches = do
        filters <- pathsOnly <$> pathFilters
        _ <- NE.nonEmpty filters
        Just $ mapMaybe (\one -> applyPathFilter one dir targets) filters

  matchesOnly <- validateOnlyTargets onlyTargetMatches onlyPathMatches targets
  -- [S.empty]
  -- -> S.empty
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
