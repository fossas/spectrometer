module App.Docs (
  userGuideUrl,
  newIssueUrl,
  fossaYmlDocUrl,
) where

import App.Version (versionNumber)
import Data.Text (Text)

sourceCodeUrl :: Text
sourceCodeUrl = "https://github.com/fossas/spectrometer"

guidePathOf :: Text -> Text -> Text
guidePathOf revision repoRelUrl = sourceCodeUrl <> "/blob/" <> revision <> repoRelUrl

userGuideUrl :: Text
userGuideUrl = guidePathOf (maybe "master" ("v" <>) versionNumber) "/docs/README.md"

fossaYmlDocUrl :: Text
fossaYmlDocUrl = guidePathOf (maybe "master" ("v" <>) versionNumber) "/docs/reference/files/fossa-yml.md"

newIssueUrl :: Text
newIssueUrl = sourceCodeUrl <> "/issues/new"
