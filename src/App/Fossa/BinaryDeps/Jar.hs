{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.BinaryDeps.Jar (resolveJar) where

import App.Fossa.BinaryDeps.Archive (extractZip, withArchive)
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics, recover)
import Control.Effect.Lift (Lift)
import Data.List (isSuffixOf, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversion (ToString (toString), ToText (toText))
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace qualified as Debug
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
resolveJar _ file | not (fileHasSuffix file [".jar", ".aar"]) = Debug.trace ("JAR strategy: skipping non-jar " <> toString file) $ pure Nothing
resolveJar root file = Debug.trace ("JAR strategy: Inspecting " <> toString file) $ do
  result <- withArchive extractZip file $ \dir -> do
    pomResult <- fromPom dir
    metaInfResult <- fromMetaInf dir
    pure (pomResult <|> metaInfResult)
  pure $ fmap (toUserDefDep root file) result

fromMetaInf :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> m (Maybe JarMetadata)
fromMetaInf dir = Debug.trace ("JAR strategy: Inspecting META-INF manifest") $ do
  content <- recover $ readContentsText (dir </> $(mkRelDir "META-INF") </> $(mkRelFile "MANIFEST.MF"))
  pure $ (metaInfManifestToMeta . parseMetaInfManifest) =<< content

parseMetaInfManifest :: Text -> Map Text Text
parseMetaInfManifest t = Map.fromList . map strip' . filter' $ map (Text.breakOn ":") (Text.lines t)
  where
    null' (a, b) = a == "" || b == ""
    strip' (a, b) = (Text.strip a, Text.strip $ Text.tail b)
    filter' pairs = filter (not . null') pairs

metaInfManifestToMeta :: Map Text Text -> Maybe JarMetadata
metaInfManifestToMeta manifest = do
  JarMetadata
    <$> (Map.lookup "Bundle-SymbolicName" manifest <|> Map.lookup "Implementation-Title" manifest)
    <*> Map.lookup "Implementation-Version" manifest
    <*> Just (Map.findWithDefault "Bundle-License" "" manifest)

fromPom :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> m (Maybe JarMetadata)
fromPom archive = do
  poms <- recover $ walk' (collectFilesNamed "pom.xml") (archive </> $(mkRelDir "META-INF"))
  parsePom $ choosePom poms

choosePom :: Maybe [Path Abs File] -> Maybe (Path Abs File)
choosePom (Just [pom]) = Just pom
choosePom poms = fmap (head . sortOn (length . toString)) poms

parsePom :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Maybe (Path Abs File) -> m (Maybe JarMetadata)
parsePom Nothing = pure Nothing
parsePom (Just file) = Debug.trace ("found POM file: " <> toString file) $ do
  (result :: Maybe RawPom) <- recover $ readContentsXML file
  pure $ fmap pomToMeta (validatePom =<< result)

toUserDefDep :: Path Abs Dir -> Path Abs File -> JarMetadata -> SourceUserDefDep
toUserDefDep root file JarMetadata{..} =
  SourceUserDefDep (renderRelative root file) jarVersion jarLicense (Just jarName) Nothing

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
