{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module CommonUtils (main) where

import Control.Carrier.Diagnostics (DiagnosticsC, runDiagnostics)
import Control.Carrier.Finally (FinallyC, runFinally)
import Control.Carrier.Lift (Lift)
import Control.Carrier.Simple (interpret)
import Control.Effect.Diagnostics (FailureBundle)
import Control.Effect.Exception (IOException)
import Control.Effect.Lift (sendIO)
import Control.Exception (try)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Conduit.Process.Typed (readProcess)
import Data.Function ((&))
import Data.String (fromString)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as T
import Effect.Exec
import Effect.Logger (LoggerC, Severity (SevDebug), withDefaultLogger)
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import Path
import Path.IO (getCurrentDir, makeAbsolute)
import Strategy.Python.Setuptools qualified as Setuptools

import App.Fossa.Analyze.Types (AnalyzeExperimentalPreferences (AnalyzeExperimentalPreferences), AnalyzeProject (analyzeProject))
import Control.Carrier.Debug (ignoreDebug)
import Control.Carrier.Reader (ReaderC, runReader)
import Data.Foldable (for_)
import System.Process.Typed (proc, setWorkingDir)
import Test.Hspec (Expectation, Spec, SpecWith, describe, expectationFailure, hspec, it, runIO, shouldBe)
import Types

{-

Blueprint. (Make it Work, Make it Simple, Make it Fast ~Enough~)

f(env, project, assertF) = in env: assertF project

Functional Requirements.
1. Test can be performed against different package manager version and pre-requisite environment
2. Test can be performed against final dependency graph
3. Test can be performed against final dependency graph, against desired configurations (command line args, config file)

Non-Functional Requirements.
1. Example test patterns are provided for common scenarios
2. Test execution is fast!

Assert
1. Assert (graph property) => Discover |> Analysis |> Graphing Dependency
2. Assert (warning or debug message) => Discover |> Analysis

-}

data FixtureEnvironment
  = NixEnvRawConfig Text
  | NixEnvSimpleConfig [Text]
  | WithDocker Text
  | WithoutNix
  deriving (Show, Eq, Ord)

withinEnvShell :: FixtureEnvironment -> Command -> Command
withinEnvShell (NixEnvSimpleConfig pkgs) cmd =
  Command
    { cmdName = "nix-shell"
    , cmdArgs = ["-p"] <> pkgs <> ["--run"] <> [cmdName cmd <> " " <> T.intercalate " " (cmdArgs cmd)]
    , cmdAllowErr = cmdAllowErr cmd
    }
withinEnvShell (NixEnvRawConfig nixExpression) cmd =
  Command
    { cmdName = "nix-shell"
    , cmdArgs = ["-I"] <> [nixExpression] <> ["--run"] <> [cmdName cmd <> " " <> T.intercalate " " (cmdArgs cmd)]
    , cmdAllowErr = cmdAllowErr cmd
    }
withinEnvShell (WithDocker dockerImage) cmd =
  Command
    { cmdName = "docker"
    , cmdArgs = ["run"] <> ["-it"] <> ["--rm"] <> [dockerImage] <> [cmdName cmd <> " " <> T.intercalate " " (cmdArgs cmd)]
    , cmdAllowErr = cmdAllowErr cmd
    }
withinEnvShell (WithoutNix) cmd = cmd

runExecIOWithinEnv ::
  ( Has (Lift IO) sig m
  ) =>
  FixtureEnvironment ->
  ExecIOC m a ->
  m a
runExecIOWithinEnv conf = interpret $ \case
  Exec dir cmd -> sendIO $ do
    absolute <-
      case dir of
        Abs absDir -> pure absDir
        Rel relDir -> makeAbsolute relDir

    let cmdInEnv = withinEnvShell conf cmd

    let cmdName' = toString $ cmdName cmdInEnv
        cmdArgs' = Prelude.map toString $ cmdArgs cmdInEnv

        mkFailure :: ExitCode -> Stdout -> Stderr -> CmdFailure
        mkFailure = CmdFailure cmdInEnv (fromAbsDir absolute)

        ioExceptionToCmdFailure :: IOException -> CmdFailure
        ioExceptionToCmdFailure = mkFailure (ExitFailure 1) "" . fromString . show

    processResult <- try $ readProcess (setWorkingDir (fromAbsDir absolute) (proc cmdName' cmdArgs'))

    -- apply business logic for considering whether exitcode + stderr constitutes a "failure"
    let mangleResult :: (ExitCode, Stdout, Stderr) -> Either CmdFailure Stdout
        mangleResult (exitcode, stdout, stderr) =
          case (exitcode, cmdAllowErr cmd) of
            (ExitSuccess, _) -> Right stdout
            (_, Never) -> Left $ mkFailure exitcode stdout stderr
            (_, NonEmptyStdout) ->
              if BL.null stdout
                then Left $ mkFailure exitcode stdout stderr
                else Right stdout
            (_, Always) -> Right stdout

    let result :: Either CmdFailure Stdout
        result = first ioExceptionToCmdFailure processResult >>= mangleResult

    pure result

main :: IO ()
main = hspec spec

type TestC m a = ExecIOC (ReadFSIOC (DiagnosticsC (LoggerC ((ReaderC (AnalyzeExperimentalPreferences)) (FinallyC m))))) a

testRunnerWithLogger :: (Has (Lift IO) sig m) => TestC m a -> FixtureEnvironment -> m (Either FailureBundle a)
testRunnerWithLogger f env =
  f
    & (runExecIOWithinEnv env)
    & runReadFSIO
    & runDiagnostics
    & withDefaultLogger SevDebug
    & runReader (AnalyzeExperimentalPreferences Nothing)
    & runFinally

withPython3 = NixEnvSimpleConfig ["python3"]

data GraphSummary = GraphSummary
  { totalNodes :: Int
  , totalEdges :: Int
  , directNodes :: Int
  }

-- TODO: HSpec equivalent? https://docs.nunit.org/articles/nunit/writing-tests/attributes/theory.html
data AnalysisIntegrationCase a = AnalysisIntegrationCase
  { caseName :: Text
  , caseBaseDir :: Path Abs Dir
  , caseDiscoveryFunction :: Path Abs Dir -> TestC IO [DiscoveredProject a]
  , caseFixtureEnvironment :: FixtureEnvironment
  -- TODO: LocalDir, GithubDownload, TarballDownload
  , caseTestProject :: Text
  -- TODO: Fix
  , asserts :: [(DiscoveredProject a -> Bool, [(Text, DiscoveredProject a -> SpecWith ())])]

  }

failed :: FailureBundle -> Expectation
failed = expectationFailure . show

mkSpecAnalysisCase :: AnalyzeProject a => AnalysisIntegrationCase a -> Spec
mkSpecAnalysisCase testCase = do
  describe (toString . caseName $ testCase) $ do
    discoveryResult <- runIO $ testRunnerWithLogger (caseDiscoveryFunction testCase $ caseBaseDir testCase) (withPython3)

    -- Mirror, mirror on the wall
    -- who is the ugliest loop of them all?

    case discoveryResult of
      Left f -> it "should discovery result" $ failed f
      Right discoveredProjects -> do
        for_ (asserts testCase) $ \(filterF, shoulds) -> do
          let foundRelProjs = filter filterF discoveredProjects
          for_ foundRelProjs $ \foundProj -> do
            analysisResult <- runIO $ testRunnerWithLogger (ignoreDebug $ analyzeProject (projectBuildTargets foundProj) (projectData foundProj)) (withPython3)
            case analysisResult of
              Left fb -> it "should analyze discovered project" $ failed fb
              Right dr -> do
                for_ shoulds $ \should -> do
                  pure should

spec :: Spec
spec = do
  -- TODO: End to End Test Example
  -- TODO: Custom Command and Configuration Test Example
  -- TODO: Strategy like transverse for Envs? -- go1.1, go1.2, go1.3, ...?

  -- Discovery Workflow
  describe "Sample discover workflow" $ do
    testdir <- runIO getCurrentDir

    it "should perform discovery" $
      do
        result <- testRunnerWithLogger (Setuptools.discover testdir) (withPython3)
        case result of
          Left failureBundle -> expectationFailure . show $ failureBundle
          Right discoveredProjects -> do
            let f = head discoveredProjects
            print $ show discoveredProjects
            res <- testRunnerWithLogger (ignoreDebug $ analyzeProject (projectBuildTargets f) (projectData f)) (withPython3)
            print $ show res

            -- Dummy assertion
            1 `shouldBe` 1
