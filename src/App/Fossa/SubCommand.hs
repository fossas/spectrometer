{-# LANGUAGE RecordWildCards #-}

module App.Fossa.SubCommand (
  runSubCommand,
  SubCommand (..),

  -- Only exported here for prototyping
  ConfigFile (..),
  Envs (..),
) where

import Control.Monad ((<=<))
import Options.Applicative (Parser, command, info, subparser, InfoMod)

data ConfigFile = ConfigFile
data Envs = Envs

data SubCommand init prepared = SubCommand
  { commandName :: String
  , commandInfo :: InfoMod init
  , parser :: Parser init
  , optMerge :: ConfigFile -> Envs -> init -> IO prepared
  , perform :: prepared -> IO ()
  }

-- For debug purposes only.  We usually won't need this.
instance Show (SubCommand a b) where
  show SubCommand{commandName} = "SubCommand: " <> commandName

runSubCommand :: forall a b. SubCommand a b -> Parser (IO ())
runSubCommand SubCommand{..} = mergeAndRun <$> subCommandParser
  where
    -- Pretend we read the config file and all relevant env vars
    mergeAndRun :: a -> IO ()
    mergeAndRun = perform <=< optMerge ConfigFile Envs

    -- We may want to also include sub-program descriptions instead of just 'mempty'
    subCommandParser :: Parser a
    subCommandParser = subparser $ command commandName $ info parser commandInfo