module Diagnostics
  ( CLIErr(..)
  , ExecErr(..)
  , ReadFSErr(..)
  ) where

import Prologue

-- | Errors that can be produced by 'Discover' and 'Strategy'

-- FIXME: remove?
data CLIErr =
    CLIErrExec ExecErr -- ^ Command execution error
  | CLIErrReadFS ReadFSErr -- ^ Filesystem error
  deriving (Eq, Ord, Show, Generic, Typeable)

data ExecErr =
    CommandFailed Text Text -- ^ Command execution failed, usually from a non-zero exit. command, stderr
  | CommandParseError Text Text -- ^ Command output couldn't be parsed. TODO: ask user to help with this. command, err
  deriving (Eq, Ord, Show, Generic, Typeable)

data ReadFSErr =
    FileReadError FilePath Text -- ^ A file couldn't be read. file, err
  | FileParseError FilePath Text -- ^ A file's contents couldn't be parsed. TODO: ask user to help with this. file, err
  | ResolveError Text -- ^ An IOException was thrown when resolving a file/directory
  deriving (Eq, Ord, Show, Generic, Typeable)
