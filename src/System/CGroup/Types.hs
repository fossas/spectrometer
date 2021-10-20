module System.CGroup.Types (
  -- * CGroup Controllers
  Controller (..),
  resolveCGroupController,

  -- * CGroups
  CGroup (..),

  -- * Mounts
  Mount (..),

  -- * Exported for testing
  findMatchingCGroup,
  findMatchingMount,
  parseMountInfo,
  parseCGroups,
) where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Data.Char (isSpace)
import Data.Foldable (find)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import System.Directory (getSymbolicLinkTarget)
import System.FilePath ((</>))
import Text.Megaparsec (Parsec, eof, manyTill, optional, parse, skipMany, some, takeWhile1P, takeWhileP)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

-- | A CGroup controller path for a specific subsystem
newtype Controller a = Controller {unController :: FilePath}
  deriving (Show)

-- | Resolve a CGroup controller by the given name
resolveCGroupController :: Text -> IO (Controller a)
resolveCGroupController controllerName = do
  cgroups <- parseFile parseCGroups cgroupPath
  mounts <- parseFile parseMountInfo mountinfoPath
  procRoot <- getSymbolicLinkTarget procRootPath

  cgroup <- maybe (fail "Couldn't find cgroup for controller") pure (findMatchingCGroup controllerName cgroups)
  mount <- maybe (fail "Couldn't find mount for cgroup") pure (findMatchingMount controllerName cgroup mounts)

  pure (Controller (procRoot </> toString (mountPoint mount)))

-- | see proc(5): filepath to a symbolic link that points to the filesystem root as viewed by this process
procRootPath :: FilePath
procRootPath = "/proc/self/root"

-- | see cgroups(7): filepath to a file that contains information about control groups applied to this process
cgroupPath :: FilePath
cgroupPath = "/proc/self/cgroup"

-- | see proc(5): filepath to a file that contains information about mounts available to this process
mountinfoPath :: FilePath
mountinfoPath = "/proc/self/mountinfo"

-- | Parse a file
parseFile :: Parser a -> FilePath -> IO a
parseFile parser file = either throwIO pure . parse parser file =<< TIO.readFile file

-- | Find a CGroup matching a controller name
--
-- For cgroups version 1, we use @containsController@ to explicitly look for the controller within a cgroup
--
-- For cgroups version 2, we use @emptyControllers@ to find a cgroup without any controllers
--
-- see cgroups(7): /proc/[pid]/cgroup section
findMatchingCGroup :: Text -> [CGroup] -> Maybe CGroup
findMatchingCGroup controllerName = find (\group -> containsController group || emptyControllers group)
  where
    containsController :: CGroup -> Bool
    containsController = (controllerName `elem`) . controlGroupControllers

    emptyControllers :: CGroup -> Bool
    emptyControllers = null . controlGroupControllers

-- | Find a Mount matching a controller name and cgroup
--
-- Per cgroups(7), the cgroup path is relative to the process filesystem root,
-- so it will match exactly with a mount root parsed from /proc/self/mountinfo
--
-- We're also looking for two cgroups-specific pieces of data in the mount:
-- - A filesystem type of "cgroup"
-- - The controller name within the "super options" or "mount source" for the mount.
--
-- The "super options" are more specific than the "mount source" within a
-- process. System-level cgroups will have controller types as "mount source",
-- and process-level cgroups put controller types in "super options"
findMatchingMount :: Text -> CGroup -> [Mount] -> Maybe Mount
findMatchingMount controllerName cgroup mounts =
  find (\mount -> matchingMountRoot mount && matchingFilesystemType mount && matchingControllerSuperOptions mount) mounts
    <|> find (\mount -> matchingMountRoot mount && matchingFilesystemType mount && matchingControllerMountSource mount) mounts
  where
    matchingMountRoot :: Mount -> Bool
    matchingMountRoot = (== controlGroupPath cgroup) . mountRoot

    matchingFilesystemType :: Mount -> Bool
    matchingFilesystemType = (== "cgroup") . mountFilesystemType

    matchingControllerSuperOptions :: Mount -> Bool
    matchingControllerSuperOptions = (controllerName `elem`) . mountSuperOptions

    matchingControllerMountSource :: Mount -> Bool
    matchingControllerMountSource = (== controllerName) . mountSource

-----

-- | A cgroup, as viewed within /proc/[pid]/cgroup
--
-- see cgroups(7): /proc/[pid]/cgroup section
data CGroup = CGroup
  { controlGroupControllers :: [Text]
  , controlGroupPath :: Text
  }
  deriving (Show)

-- | Parse an entire /proc/[pid]/cgroup file into a list of cgroups
parseCGroups :: Parser [CGroup]
parseCGroups = some parseSingleCGroup <* eof

-- | Parse a single cgroup line within /proc/[pid]/cgroup
--
-- hierarchyID:list,of,controllers:path
--
-- In cgroups version 1, a comma-separated list of controllers exists within each group
--
-- In cgroups version 2, the "controllers" section is always an empty string
--
-- see cgroups(7): /proc/[pid]/cgroup section
parseSingleCGroup :: Parser CGroup
parseSingleCGroup =
  CGroup
    <$ takeUntil1P ':' -- ignore hierarchy ID number
    <*> (splitOnIgnoreEmpty "," <$> takeUntilP ':') -- comma-separated list of controllers
    <*> takeUntil1P '\n' -- path

-- return the prefix of the input until reaching the supplied character.
-- the character is also consumed as part of this parser.
--
-- this parser succeeds even when the character does not exist in the input
takeUntilP :: Char -> Parser Text
takeUntilP c = takeWhileP Nothing (/= c) <* optional (char c)

-- like 'takeUntilP', but expects a non-empty prefix before the character
takeUntil1P :: Char -> Parser Text
takeUntil1P c = takeWhile1P Nothing (/= c) <* optional (char c)

-- Data.Text.splitOn, but returns empty list on empty haystack, rather than [""]
--
-- >>> Data.Text.splitOn "foo" ""
-- [""]
--
-- >>> splitOnIgnoreEmpty "foo" ""
-- []
splitOnIgnoreEmpty :: Text -> Text -> [Text]
splitOnIgnoreEmpty _ "" = []
splitOnIgnoreEmpty s str = Text.splitOn s str

--------------

-- | A mount, as viewed within /proc/[pid]/mountinfo
--
-- see proc(5): /proc/[pid]/mountinfo section
data Mount = Mount
  { mountId :: Text
  , mountParentId :: Text
  , mountStDev :: Text
  , mountRoot :: Text
  , mountPoint :: Text
  , mountOptions :: Text
  , mountTags :: [Text]
  , mountFilesystemType :: Text
  , mountSource :: Text
  , mountSuperOptions :: [Text]
  }
  deriving (Show)

-- | Parse an entire /proc/[pid]/mountinfo file into a list of mounts
parseMountInfo :: Parser [Mount]
parseMountInfo = some parseSingleMount <* eof

-- | Parse a single mount line within /proc/[pid]/mountinfo
--
-- Fields are space-separated
--
-- see proc(5): /proc/[pid]/mountinfo section
parseSingleMount :: Parser Mount
parseSingleMount =
  Mount
    <$> field -- id
    <*> field -- parent id
    <*> field -- st_dev
    <*> field -- mount root
    <*> field -- mount point
    <*> field -- mount options
    <*> field `manyTill` separator -- optional mount tags, terminated by "-"
    <*> field -- filesystem type
    <*> field -- mount source
    <*> (splitOnIgnoreEmpty "," <$> field) -- super options
    <* optional (char '\n')

type Parser = Parsec Void Text

-- a field in the mountinfo file, terminated by whitespace
field :: Parser Text
field = lexeme $ takeWhile1P Nothing (not . isSpace)

-- separator after optional mount tags ("-")
separator :: Parser Char
separator = lexeme $ char '-'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (skipMany (char ' '))
