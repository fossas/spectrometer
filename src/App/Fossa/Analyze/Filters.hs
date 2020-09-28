{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze.Filters
  ( BuildTargetFilter (..),
    filterParser,
    applyFilter,
    applyFilters,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Path
import Path.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import Types (BuildTarget (..), NewProject (..))

data BuildTargetFilter
  = -- | buildtool, directory. if a project matches this filter, all of its
    -- buildtargets will be analyzed
    ProjectFilter Text (Path Rel Dir)
  | -- | buildtool, directory, buildtarget. if a target matches this filter,
    -- only that target will be analyzed
    TargetFilter Text (Path Rel Dir) BuildTarget
  deriving (Eq, Ord, Show)

applyFilters :: Path Abs Dir -> [BuildTargetFilter] -> NewProject m -> Maybe (Set BuildTarget)
applyFilters _ [] project = Just (projectBuildTargets project)
applyFilters basedir filters NewProject {..} = do
  rel <- makeRelative basedir projectPath

  let individualResults = mapMaybe (\one -> applyFilter one projectType rel projectBuildTargets) filters
  successful <- NE.nonEmpty $ individualResults

  pure (sconcat successful)

-- | Apply the filter, returning the set of buildtargets that succeed
applyFilter :: BuildTargetFilter -> Text -> Path Rel Dir -> Set BuildTarget -> Maybe (Set BuildTarget)
applyFilter (ProjectFilter tool dir) tool' dir' targets
  | tool == tool',
    dir == dir' =
    Just targets
  | otherwise = Nothing
applyFilter (TargetFilter tool dir target) tool' dir' targets
  | tool == tool',
    dir == dir',
    S.member target targets =
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
