{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.BinaryDeps.Jar (resolveJar) where

import App.Fossa.BinaryDeps.Archive (extractZip, withArchive)
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics, context, fromMaybeText, recover, (<||>))
import Control.Effect.Lift (Lift)
import Data.List (isSuffixOf, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToString (toString), ToText (toText))
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipAll), findFileNamed, walk')
import Effect.ReadFS (ReadFS, readContentsText, readContentsXML)
import GHC.Base ((<|>))
import Path (Abs, Dir, File, Path, filename, mkRelDir, mkRelFile, stripProperPrefix, toFilePath, (</>))
import Srclib.Types (SourceUserDefDep (..))
import Strategy.Maven.Pom.PomFile (MavenCoordinate (..), Pom (..), RawPom, pomLicenseName, validatePom)

data JarMetadata = JarMetadata
  { jarName :: Text
  , jarVersion :: Text
  , jarLicense :: Text
  }

-- | Implement JAR resolution using a similar method to Ant analysis in CLIv1
resolveJar :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> Path Abs File -> m (Maybe SourceUserDefDep)
resolveJar _ file | not (fileHasSuffix file [".jar", ".aar"]) = pure Nothing
resolveJar root file = do
  result <- recover $ context ("Infer metadata from " <> toText file) $ withArchive extractZip file $ \dir -> fromPom dir <||> fromMetaInf dir
  pure $ fmap (toUserDefDep root file) result

fromMetaInf :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> m JarMetadata
fromMetaInf archive = context ("Parse " <> toText metaInfPath) $ do
  content <- readContentsText metaInfPath
  metaInfManifestToMeta $ parseMetaInfManifest content
  where
    metaInfPath = archive </> $(mkRelDir "META-INF") </> $(mkRelFile "MANIFEST.MF")

parseMetaInfManifest :: Text -> Map Text Text
parseMetaInfManifest t = Map.fromList . map strip' . filter' $ map (Text.breakOn ":") (Text.lines t)
  where
    null' (a, b) = any Text.null [a, b]
    strip' (a, b) = (Text.strip a, Text.strip $ Text.tail b)
    filter' pairs = filter (not . null') pairs

metaInfManifestToMeta :: Has Diagnostics sig m => Map Text Text -> m JarMetadata
metaInfManifestToMeta manifest =
  JarMetadata
    <$> fromMaybeText "Missing bundle name" (Map.lookup "Bundle-SymbolicName" manifest <|> Map.lookup "Implementation-Title" manifest)
    <*> fromMaybeText "Missing implementation version" (Map.lookup "Implementation-Version" manifest)
    <*> pure (Map.findWithDefault "Bundle-License" "" manifest)

fromPom :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> m JarMetadata
fromPom archive = context ("Parse representative pom.xml in " <> toText archive) $ do
  poms <- context "Find pom.xml files" $ walk' (collectFilesNamed "pom.xml") (archive </> $(mkRelDir "META-INF"))
  pom <- fromMaybeText "No pom.xml files found" $ choosePom poms
  parsePom pom

choosePom :: [Path Abs File] -> Maybe (Path Abs File)
choosePom [] = Nothing
choosePom [pom] = Just pom
choosePom poms = Just . head $ sortOn (length . toString) poms

parsePom :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m JarMetadata
parsePom file = context ("Parse pom file: " <> toText file) $ do
  (result :: RawPom) <- readContentsXML file
  validated <- fromMaybeText "Invalid format" $ validatePom result
  pure $ pomToMeta validated

pomToMeta :: Pom -> JarMetadata
pomToMeta Pom{..} = do
  let name = (coordGroup pomCoord) <> ":" <> (coordArtifact pomCoord)
  let license = Text.intercalate "\n" $ mapMaybe pomLicenseName pomLicenses
  JarMetadata name (coordVersion pomCoord) license

collectFilesNamed :: Applicative f => String -> Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> f ([Path Abs File], WalkStep)
collectFilesNamed name _ _ files = case findFileNamed name files of
  Just f -> pure ([f], WalkSkipAll)
  Nothing -> pure ([], WalkContinue)

fileHasSuffix :: Path a File -> [String] -> Bool
fileHasSuffix file = any (\suffix -> suffix `isSuffixOf` toString (filename file))

renderRelative :: Path Abs Dir -> Path Abs File -> Text
renderRelative absDir absFile =
  case stripProperPrefix absDir absFile of
    Left _ -> toText . toFilePath $ absFile
    Right relFile -> toText . toFilePath $ relFile

toUserDefDep :: Path Abs Dir -> Path Abs File -> JarMetadata -> SourceUserDefDep
toUserDefDep root file JarMetadata{..} =
  SourceUserDefDep (renderRelative root file) jarVersion jarLicense (Just jarName) Nothing
