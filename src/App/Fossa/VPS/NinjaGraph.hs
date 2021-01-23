{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.NinjaGraph
  ( ninjaGraphMain,
    NinjaGraphOptions (..),
  )
where

import App.Fossa.VPS.Ninja.Env (newEnv)
import App.Fossa.VPS.Ninja.Parse (parse)
import App.Fossa.VPS.Ninja.Type (Ninja (..))
import App.Types (BaseDir (..), OverrideProject (..), ProjectRevision (..))
import Control.Carrier.Diagnostics (Diagnostics, fatalText, runDiagnostics, withResult)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (unless)
import Data.Text (Text, pack)
import Data.Time (getCurrentTime, getCurrentTimeZone, utcToZonedTime)
import Effect.Logger (Severity, withLogger)
import Effect.ReadFS (ReadFS, doesFileExist, resolveFile, runReadFSIO)
import Fossa.API.Types (ApiOpts)
import Path (Abs, File, Path, toFilePath)
import System.Exit (exitSuccess)
import App.Fossa.ProjectInference (inferProject, mergeOverride)
import App.Fossa.VPS.Scan.Core (createLocator, getSherlockInfo, SherlockInfo(..))
import App.Fossa.VPS.Scan.ScotlandYard (uploadBuildGraph, ScotlandYardNinjaOpts(..))

data NinjaGraphOptions = NinjaGraphOptions
  { -- TODO: These three fields seem fairly common. Factor out into `CommandOptions t`?
    ngoLogSeverity :: Severity,
    ngoAPIOptions :: ApiOpts,
    ngoProjectOverride :: OverrideProject,
    --
    ngoAndroidTopDir :: BaseDir,
    ngoLunchCombo :: Text,
    ngoScanID :: Text,
    ngoBuildName :: Text
  }

ninjaGraphMain :: NinjaGraphOptions -> IO ()
ninjaGraphMain n@NinjaGraphOptions {..} =
  withLogger ngoLogSeverity $ do
    result <- runReadFSIO $ runDiagnostics $ ninjaGraph n
    withResult ngoLogSeverity result pure

ninjaFileNotFoundError :: Path Abs File -> Text
ninjaFileNotFoundError p =
  pack $
    unlines
      [ "Could not find the generated Ninja build file at " <> show p <> ".",
        "",
        "Debugging checklist:",
        "",
        "- Did you run an Android build?",
        "  If so, this file should be generated. Check if it exists.",
        "",
        "- Is base directory set to the root of the Android build?",
        "  This is the positional argument passed to the CLI. By default, this is the current working directory.",
        "",
        "- Are you using a custom $OUT directory for your Android build?",
        "  This is unsupported. Rename your $OUT directory to \"//out\"."
      ]

ninjaGraph :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => NinjaGraphOptions -> m ()
ninjaGraph NinjaGraphOptions {..} = withLogger ngoLogSeverity $ do
  -- Resolve the Ninja files from the Android repository's root.
  let (BaseDir top) = ngoAndroidTopDir
  soongNinjaFilePath <- resolveFile top "out/soong/build.ninja"
  katiNinjaFilePath <- resolveFile top $ "out/build-" <> ngoLunchCombo <> ".ninja"

  -- Check if the files exist.
  ok <- doesFileExist soongNinjaFilePath
  unless ok $ fatalText $ ninjaFileNotFoundError soongNinjaFilePath
  ok' <- doesFileExist katiNinjaFilePath
  unless ok' $ fatalText $ ninjaFileNotFoundError katiNinjaFilePath

  -- Parse the Ninja files.
  tz <- sendIO getCurrentTimeZone
  let showT = show . utcToZonedTime tz

  t1 <- sendIO getCurrentTime
  sendIO $ putStrLn $ "RUNNING PARSE " <> showT t1
  e1 <- sendIO newEnv
  n1 <- sendIO $ parse (toFilePath soongNinjaFilePath) e1
  let Ninja {..} = n1
  sendIO $ putStrLn $ "RULES: " <> show (length rules)
  sendIO $ putStrLn $ "RULES: " <> show (take 1 rules)

  sendIO $ putStrLn $ "SINGLES: " <> show (length singles)
  sendIO $ putStrLn $ "SINGLES: " <> show (take 1 singles)

  sendIO $ putStrLn $ "MULTIPLES: " <> show (length multiples)
  sendIO $ putStrLn $ "MULTIPLES: " <> show (take 1 multiples)

  sendIO $ putStrLn $ "PHONYS: " <> show (length phonys)
  sendIO $ putStrLn $ "PHONYS: " <> show (take 1 phonys)

  sendIO $ putStrLn $ "DEFAULTS: " <> show (length defaults)
  sendIO $ putStrLn $ "DEFAULTS: " <> show defaults

  sendIO $ putStrLn $ "POOLS: " <> show (length pools)
  sendIO $ putStrLn $ "POOLS: " <> show pools

  -- t3 <- sendIO getCurrentTime
  -- sendIO $ putStrLn $ "RUNNING SOONG PARSE " <> showT t3
  -- e2 <- sendIO newEnv
  -- n2 <- sendIO $ parse (toFilePath soongNinjaFilePath) e2
  -- sendIO $ print n2

  t4 <- sendIO getCurrentTime
  sendIO $ putStrLn $ "DONE " <> showT t4
  _ <- sendIO exitSuccess

  -- Upload the parsed Ninja files.

  -- ProjectRevision {..} <- mergeOverride ngoProjectOverride <$> inferProject (unBaseDir ngoAndroidTopDir)
  -- SherlockInfo {..} <- getSherlockInfo ngoAPIOptions
  -- let locator = createLocator ninjaProjectName sherlockOrgId
  --     syOpts = ScotlandYardNinjaOpts locator sherlockOrgId ninjaGraphOpts
  -- _ <- uploadBuildGraph ngoAPIOptions syOpts graph
  pure ()

-- result <- runDiagnostics $ getAndParseNinjaDeps undefined undefined undefined
-- case result of
--   Left failure -> do
--     sendIO . print $ renderFailureBundle failure
--     sendIO exitFailure
--   Right _ -> pure ()

-- getAndParseNinjaDeps :: (Has Diagnostics sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> NinjaGraphOpts -> m ()
-- getAndParseNinjaDeps dir apiOpts ninjaGraphOpts@NinjaGraphOpts {..} = do
