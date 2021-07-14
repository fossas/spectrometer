{-# LANGUAGE TemplateHaskell #-}

module Discovery.Filters (
  BuildTargetFilterOld (..),
  AllFilters (..),
  filterParser,
  applyFilters,
  configTargetToFilter,
  TargetFilter (..),
  FilterCombination (..),
  FilterMatch (..),
  FilterResult (..),
  apply,
) where

import App.Fossa.Configuration
import Data.List ((\\))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import Data.Set qualified as S
import Data.Set.NonEmpty
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Path
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, try),
  Parsec,
  satisfy,
  some,
  (<|>),
 )
import Text.Megaparsec.Char
import Types (BuildTarget (..), FoundTargets (FoundTargets, ProjectWithoutTargets))

data AllFilters = AllFilters
  { legacyFilters :: [BuildTargetFilterOld]
  , includeFilters :: FilterCombination
  , excludeFilters :: FilterCombination
  }

data BuildTargetFilterOld
  = -- | buildtool, directory. if a project matches this filter, all of its
    -- build targets will be analyzed
    ProjectFilter Text (Path Rel Dir)
  | -- | buildtool, directory, build target. if a target matches this filter,
    -- only that target will be analyzed
    TargetFilter Text (Path Rel Dir) BuildTarget
  deriving (Eq, Ord, Show)

data TargetFilter = TypeTarget Text | TypeDirTarget Text (Path Rel Dir) | TypeDirTargetTarget Text (Path Rel Dir) BuildTarget

configTargetToFilter :: ConfigTarget -> TargetFilter
configTargetToFilter (ConfigTarget toolType target) = case target of
  Nothing -> TypeTarget toolType
  Just (DirectoryFilter dir) -> TypeDirTarget toolType dir
  Just (ExactTargetFilter dir exactTarget) -> TypeDirTargetTarget toolType dir (BuildTarget exactTarget)

data FilterCombination = FilterCombination
  { combinedTargets :: [TargetFilter]
  , combinedPaths :: [Path Rel Dir]
  }

applyFilters :: AllFilters -> Text -> Path Rel Dir -> FoundTargets -> Maybe FoundTargets
applyFilters (AllFilters [] onlyFilters excludeFilters) tool dir targets = finalizeResult (apply onlyFilters excludeFilters tool dir) targets
applyFilters (AllFilters legacyFilters _ _) tool dir targets = finalizeResult (apply legacyOnlyFilters (FilterCombination [] []) tool dir) targets
  where
    legacyOnlyFilters = FilterCombination (legacyFiltersToTargetFilter <$> legacyFilters) []
    legacyFiltersToTargetFilter :: BuildTargetFilterOld -> TargetFilter
    legacyFiltersToTargetFilter (ProjectFilter t path)  = TypeDirTarget t path
    legacyFiltersToTargetFilter (TargetFilter t path target) = TypeDirTargetTarget t path target

finalizeResult :: FilterResult -> FoundTargets -> Maybe FoundTargets
finalizeResult ResultNone _ = Nothing
finalizeResult _ ProjectWithoutTargets = Just ProjectWithoutTargets
finalizeResult ResultAll targets = Just targets
finalizeResult (ResultInclude found) (FoundTargets targets) = (Just . FoundTargets) =<< nonEmpty (S.intersection (toSet targets) $ S.fromList $ NE.toList found)
finalizeResult (ResultExclude found) (FoundTargets targets) = (Just . FoundTargets) =<< nonEmpty (S.difference (toSet targets) $ S.fromList $ NE.toList found)

-- (buildTargetFilters-only `union` pathFilters-only)
--   `subtract` (buildTargetFilters-exclude `union` pathFilters-exclude)
apply :: FilterCombination -> FilterCombination -> Text -> Path Rel Dir -> FilterResult
apply include exclude buildtool dir =
  dSubtract
    (fromMaybe MatchAll (applyComb include buildtool dir))
    (fromMaybe MatchNone (applyComb exclude buildtool dir))

-- Nothing = "Unknown" -- i.e., there were no filters
applyComb :: FilterCombination -> Text -> Path Rel Dir -> Maybe FilterMatch
applyComb comb buildtool dir =
  buildTargetFiltersResult <> pathFiltersResult
  where
    buildTargetFiltersResult :: Maybe FilterMatch
    buildTargetFiltersResult = foldMap' (\t -> applyTarget t buildtool dir) (combinedTargets comb)

    pathFiltersResult :: Maybe FilterMatch
    pathFiltersResult = foldMap' (`applyPath` dir) (combinedPaths comb)

-- mvn@foo:bar
-- mvn@foo ProjectWithoutTargets -> MatchNone
-- mvn@foo (FoundTargets xs) -> bar `elem` xs -> MatchSome [bar] otherwise MatchNone
applyTarget :: TargetFilter -> Text -> Path Rel Dir -> FilterMatch
applyTarget (TypeTarget t) u _ = if t == u then MatchAll else MatchNone
applyTarget (TypeDirTarget t p) u q = if t == u && p == q then MatchAll else MatchNone
applyTarget (TypeDirTargetTarget t p target) u q = if t == u && p == q then MatchSome (target NE.:| []) else MatchNone

-- (parent path) (child path)
applyPath :: Path Rel Dir -> Path Rel Dir -> FilterMatch
applyPath t u = if isProperPrefixOf t u || t == u then MatchAll else MatchNone

-- MatchNone <> MatchAll = MatchAll is the reason for this order
instance Semigroup FilterMatch where
  MatchNone <> t = t
  t <> MatchNone = t
  t <> MatchAll = t
  MatchAll <> t = t
  MatchSome ts <> MatchSome us = MatchSome (ts <> us)

-- | 'foldMap', but only requires a 'Semigroup' instance.
--
-- When the provided list is empty, this returns 'Nothing'
foldMap' :: Semigroup s => (a -> s) -> [a] -> Maybe s
foldMap' f xs = sconcat <$> NE.nonEmpty (map f xs)

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

data FilterMatch = MatchNone | MatchAll | MatchSome (NE.NonEmpty BuildTarget)
  deriving (Eq, Ord, Show)

data FilterResult = ResultNone | ResultAll | ResultInclude (NE.NonEmpty BuildTarget) | ResultExclude (NE.NonEmpty BuildTarget)
  deriving (Eq, Ord, Show)

-- dSubtract -> IncludeMatches -> ExcludeMatches -> FilterResult
-- dSubtract defines how different types of include and exclude matches are merged to create a FilterResult.
dSubtract :: FilterMatch -> FilterMatch -> FilterResult
dSubtract _ MatchAll = ResultNone
dSubtract MatchNone _ = ResultNone
dSubtract MatchAll MatchNone = ResultAll
dSubtract MatchAll (MatchSome xs) = ResultExclude xs
dSubtract (MatchSome xs) MatchNone = ResultInclude xs
dSubtract (MatchSome xs) (MatchSome ys) = maybe ResultNone ResultInclude (neDifference xs ys)

-- | Compute the difference of two non-empty lists
neDifference :: Eq a => NE.NonEmpty a -> NE.NonEmpty a -> Maybe (NE.NonEmpty a)
neDifference xs ys = NE.nonEmpty (NE.toList xs \\ NE.toList ys)

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
