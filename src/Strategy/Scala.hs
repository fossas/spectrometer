module Strategy.Scala
  ( discover,
  )
where

import Control.Effect.Error
import Control.Effect.Output
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Graphing (Graphing)
import Prologue
import Strategy.Maven.Pom (mkProjectClosure)
import Strategy.Maven.Pom.Closure (MavenProjectClosure (..), buildProjectClosures)
import Strategy.Maven.Pom.Resolver (GlobalClosure (..), buildGlobalClosure)
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover basedir =
  walk
    ( \_ _ files ->
        case find (\f -> fileName f == "build.sbt" || fileName f == "build.scala") files of
          Nothing -> pure WalkContinue
          Just file -> do
            runStrategy "scala-sbt" ScalaGroup (analyze basedir file)
            pure WalkSkipAll
    )
    basedir

makePomCmd :: Command
makePomCmd =
  Command
    { cmdNames = ["sbt"],
      cmdBaseArgs = ["makePom", "-no-colors"],
      cmdAllowErr = Never
    }

analyze ::
  ( Has Exec sig m,
    Has (Error ExecErr) sig m,
    Has ReadFS sig m,
    Effect sig,
    Has (Output ProjectClosure) sig m
  ) =>
  Path Abs Dir ->
  Path Rel File ->
  m ()
analyze basedir file = do
  stdoutBL <- execThrow (parent file) makePomCmd []

  let stdoutLText = decodeUtf8 stdoutBL
      stdout = TL.toStrict stdoutLText
      --
      stdoutLines :: [Text]
      stdoutLines = T.lines stdout
      --
      pomLines :: [Text]
      pomLines = catMaybes $ map (T.stripPrefix "[info] Wrote ") stdoutLines
      --
      pomLocations :: Maybe [Path Abs File]
      pomLocations = traverse (parseAbsFile . T.unpack) pomLines

  case pomLocations of
    Nothing -> throwError (CommandParseError "sbt makePom" "Could not parse pom locations")
    Just [] -> throwError (CommandParseError "sbt makePom" "No projects found")
    Just paths -> do
      globalClosure <- buildGlobalClosure paths
      let projects = buildProjectClosures basedir globalClosure
      traverse_ (output . mkProjectClosure) projects
