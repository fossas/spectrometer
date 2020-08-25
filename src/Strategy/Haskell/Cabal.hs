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
      <*> (obj .:? "depends" .!= S.empty)
      <*> (obj .:? "style" >>= traverse parsePlanStyle)
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
installPlanDepends InstallPlan {..} = planComponents <> planDepends

isDirectDep :: InstallPlan -> Bool
isDirectDep InstallPlan {..} = planStyle == Just Local && planType == Configured

parsePlanStyle :: MonadFail f => Text -> f (PlanStyle)
parsePlanStyle style = case T.toLower style of
  "global" -> pure Global
  "local" -> pure Local
  _ -> fail $ "unknown install plan style" ++ T.unpack style

parsePlanType :: MonadFail m => Text -> m PlanType
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
isCabalFile file = isDotCabal || isCabalDotProject
  where
    name = fileName file
    isDotCabal = ".cabal" `isSuffixOf` name
    isCabalDotProject = "cabal.project" == name

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
    when (isDirectDep plan) (direct dep)

shouldInclude :: InstallPlan -> Bool
shouldInclude plan = not $ isDirectDep plan || planType plan == PreExisting

ignorePlanId :: PlanId -> InstallPlan -> InstallPlan
ignorePlanId = flip const

buildGraph :: Has Diagnostics sig m => BuildPlan -> m (Graphing Dependency)
buildGraph plan = do
  result <- withMapping ignorePlanId $ traverse doGraph (installPlans plan)
  case result of
    Left err -> fatal err
    Right gr -> pure . G.gmap toDependency $ G.filter shouldInclude gr

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
  plans <- readContentsJson @BuildPlan (dir </> cabalPlanFilePath)
  buildGraph plans
