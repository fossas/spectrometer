{-# LANGUAGE TemplateHaskell #-}

module Strategy.Haskell.Cabal
  ( discover,

    -- * Testing
    BuildPlan (..),
    PlanId (..),
    InstallPlan (..),
    PlanStyle (..),
    PlanType (..),
    buildGraph,
  )
where

import Control.Effect.Diagnostics
import Data.Aeson.Types
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Effect.ReadFS
import qualified Graphing as G
import Graphing (Graphing)
import Prologue
import Types

newtype BuildPlan = BuildPlan {installPlans :: [InstallPlan]} deriving (Eq, Ord, Show)

newtype PlanId = PlanId {unPlanId :: Text} deriving (FromJSON, Eq, Ord, Show)

data InstallPlan
  = InstallPlan
      { planType :: PlanType,
        planId :: PlanId,
        planName :: Text,
        planVersion :: Text,
        planDepends :: Set PlanId,
        planStyle :: Maybe PlanStyle,
        planComponents :: Set PlanId
      }
  deriving (Eq, Ord, Show)

data PlanStyle
  = Local
  | Global
  deriving (Eq, Ord, Show)

data PlanType
  = PreExisting
  | Configured
  deriving (Eq, Ord, Show)

instance FromJSON BuildPlan where
  parseJSON = withObject "BuildPlan" $ \obj ->
    BuildPlan <$> obj .: "install-plan"

instance FromJSON InstallPlan where
  parseJSON = withObject "InstallPlan" $ \obj ->
    InstallPlan <$> (obj .: "type" >>= parsePlanType)
      <*> obj .: "id"
      <*> obj .: "pkg-name"
      <*> obj .: "pkg-version"
      <*> (fromMaybe mempty <$> obj .:? "depends")
      <*> (obj .:? "style" >>= parsePlanStyle)
      <*> (obj .:? "components" >>= mergeComponents)

mergeComponents :: Maybe (Map Text (Map Text (Set Text))) -> Parser (Set PlanId)
mergeComponents Nothing = pure mempty
mergeComponents (Just mapA) = do
  nested <- traverse getDepends $ M.elems mapA
  pure $ S.unions nested
  where
    getDepends :: Map Text (Set Text) -> Parser (Set PlanId)
    getDepends mapB = pure . S.map PlanId . fromMaybe mempty $ M.lookup "depends" mapB

installPlanDepends :: InstallPlan -> Set PlanId
installPlanDepends InstallPlan {..} =
  if planType == PreExisting
    then mempty
    else planComponents <> planDepends

isDirectDep :: InstallPlan -> Bool
isDirectDep InstallPlan {..} = planStyle == Just Local && planType == Configured

parsePlanStyle :: Maybe Text -> Parser (Maybe PlanStyle)
parsePlanStyle Nothing = pure Nothing
parsePlanStyle (Just style) = case T.toLower style of
  "global" -> pure $ Just Global
  "local" -> pure $ Just Local
  _ -> fail $ "unknown install plan style" ++ T.unpack style

parsePlanType :: Text -> Parser PlanType
parsePlanType typ = case T.toLower typ of
  "configured" -> pure Configured
  "pre-existing" -> pure PreExisting
  _ -> fail $ "unknown install plan type" ++ T.unpack typ

cabalGenPlanCmd :: Command
cabalGenPlanCmd =
  Command
    { cmdName = "cabal",
      cmdArgs = ["v2-build", "--dry-run"],
      cmdAllowErr = Never
    }

cabalPlanFilePath :: Path Rel File
cabalPlanFilePath = $(mkRelFile "dist-newstyle/cache/plan.json")

isCabalFile :: Path Abs File -> Bool
isCabalFile file = is_v1 || is_v2
  where
    name = fileName file
    is_v2 = ".cabal" `isSuffixOf` name
    is_v1 = "cabal.project" == name

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files ->
  if any isCabalFile files
    then do
      runSimpleStrategy "haskell-cabal" HaskellGroup $ fmap (mkProjectClosure dir) (analyze dir)
      pure WalkSkipAll
    else pure WalkContinue

mkProjectClosure :: Path Abs Dir -> Graphing Dependency -> ProjectClosureBody
mkProjectClosure dir graph =
  ProjectClosureBody
    { bodyModuleDir = dir,
      bodyDependencies = dependencies,
      bodyLicenses = []
    }
  where
    dependencies =
      ProjectDependencies
        { dependenciesGraph = graph,
          dependenciesOptimal = Optimal,
          dependenciesComplete = Complete
        }

doGraph :: Has (MappedGrapher PlanId InstallPlan) sig m => InstallPlan -> m ()
doGraph plan = do
  let parentId = planId plan
  mapping parentId plan
  for_ (installPlanDepends plan) $ \dep -> do
    edge parentId dep
    if isDirectDep plan
      then direct dep
      else pure ()

shouldExclude :: InstallPlan -> Bool
shouldExclude plan = not $ isDirectDep plan || planType plan == PreExisting

ignorePlanId :: PlanId -> InstallPlan -> InstallPlan
ignorePlanId _ b = b

buildGraph :: Has Diagnostics sig m => BuildPlan -> m (Graphing Dependency)
buildGraph plan = do
  result <- withMapping ignorePlanId $ traverse doGraph (installPlans plan)
  case result of
    Left err -> fatal err
    Right gr -> pure . G.gmap toDependency $ G.filter shouldExclude gr

toDependency :: InstallPlan -> Dependency
toDependency plan =
  Dependency
    { dependencyType = HaskellType,
      dependencyName = planName plan,
      dependencyVersion = Just $ CEq $ planVersion plan,
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }

analyze :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Graphing Dependency)
analyze dir = do
  _ <- execThrow dir cabalGenPlanCmd
  plans <- readContentsJson@BuildPlan (dir </> cabalPlanFilePath)
  buildGraph plans
