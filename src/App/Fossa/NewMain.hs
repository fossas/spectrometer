module App.Fossa.NewMain (
  appMain,
) where

import Options.Applicative

import App.Fossa.NewAnalyze (analyzeCommand)
import App.Fossa.NewTest
import App.Fossa.SubCommand
import Control.Monad (join)
import Data.Foldable (asum)

-- One annoying issue with using Alternative is that it fucks the global help text.
-- The command-individual help texts seem to be fine.
-- Also we probably need to fill out program data instead of just using mempty
appMain :: IO ()
appMain = join $ customExecParser mainPrefs $ info (asum subcommands <**> helper) mempty

mainPrefs :: ParserPrefs
mainPrefs =
  prefs $
    mconcat
      [ showHelpOnError
      , showHelpOnEmpty
      , subparserInline
      , helpShowGlobals
      ]

-- We could probably make this better with impredicative types or a GADT.  Since
-- we don't share this list, we can live with the duplication of 'runSubCommand'.
subcommands :: [Parser (IO ())]
subcommands =
  [ runSubCommand analyzeCommand
  , runSubCommand testCommand
  ]
