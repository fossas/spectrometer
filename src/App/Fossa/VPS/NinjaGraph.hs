{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.NinjaGraph
  ( ninjaGraphMain,
    NinjaGraphOptions (..),
  )
where

import App.Fossa.ProjectInference (inferProject, mergeOverride)
import App.Fossa.VPS.Ninja.Env (newEnv)
import App.Fossa.VPS.Ninja.Parse (parse)
import App.Fossa.VPS.Ninja.Type (Build (..), Ninja (..), Outputs (..))
import App.Fossa.VPS.Scan.Core (SherlockInfo (..), createLocator, getSherlockInfo)
import App.Fossa.VPS.Scan.ScotlandYard (uploadBuildGraph)
import App.Fossa.VPS.Types (NinjaGraphOptions (..))
import App.Types (BaseDir (..), ProjectRevision (..))
import Control.Carrier.Diagnostics (Diagnostics, fatalText, runDiagnostics, withResult)
import Control.Effect.Lift (Has, Lift, sendIO)
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Effect.Logger (Logger, withLogger)
import Effect.ReadFS (ReadFS, doesFileExist, resolveFile, runReadFSIO)
import GHC.Generics (Generic)
import Path (Abs, File, Path, toFilePath)

ninjaGraphMain :: NinjaGraphOptions -> IO ()
ninjaGraphMain n@NinjaGraphOptions {..} =
  withLogger ngoLogSeverity $ do
    result <- runReadFSIO $ runDiagnostics $ ninjaGraph n
    withResult ngoLogSeverity result pure

ninjaGraph :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => NinjaGraphOptions -> m ()
ninjaGraph n@NinjaGraphOptions {..} = do
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
  soongN <- sendIO $ parseNinja soongNinjaFilePath
  katiN <- sendIO $ parseNinja katiNinjaFilePath

  -- Upload the parsed Ninja files.
  ProjectRevision {..} <- mergeOverride ngoProjectOverride <$> inferProject (unBaseDir ngoAndroidTopDir)
  SherlockInfo {..} <- getSherlockInfo ngoAPIOptions
  let locator = createLocator projectName sherlockOrgId
  uploadBuildGraph n locator $ soongN <> katiN

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

parseNinja :: (MonadIO m) => Path Abs File -> m [NinjaJSON]
parseNinja filepath = do
  e <- liftIO newEnv
  n <- liftIO $ parse (toFilePath filepath) e
  return $ toSerializable n

data NinjaJSON = BuildStmt
  { rule :: Text,
    outs :: [Text],
    outsImplicit :: [Text],
    ins :: [Text],
    insImplicit :: [Text],
    insOrderOnly :: [Text]
  }
  deriving (Show, Generic)

instance ToJSON NinjaJSON where
  toEncoding = genericToEncoding defaultOptions

toSerializable :: Ninja -> [NinjaJSON]
toSerializable Ninja {..} = singles' <> multiples' <> phonys'
  where
    singleF (outputFile, Build {..}) =
      BuildStmt
        { rule = decodeUtf8 ruleName,
          outs = [decodeUtf8 outputFile],
          outsImplicit = [],
          ins = decodeUtf8 <$> depsNormal,
          insImplicit = decodeUtf8 <$> depsImplicit,
          insOrderOnly = decodeUtf8 <$> depsOrderOnly
        }
    singles' = singleF <$> singles

    multipleF (Outputs {..}, Build {..}) =
      BuildStmt
        { rule = decodeUtf8 ruleName,
          outs = decodeUtf8 <$> outsNormal,
          outsImplicit = decodeUtf8 <$> outsImplicit,
          ins = decodeUtf8 <$> depsNormal,
          insImplicit = decodeUtf8 <$> depsImplicit,
          insOrderOnly = decodeUtf8 <$> depsOrderOnly
        }
    multiples' = multipleF <$> multiples

    phonysF (outputFile, inputFiles) =
      BuildStmt
        { rule = "phony",
          outs = [decodeUtf8 outputFile],
          outsImplicit = [],
          ins = decodeUtf8 <$> inputFiles,
          insImplicit = [],
          insOrderOnly = []
        }
    phonys' = phonysF <$> phonys
