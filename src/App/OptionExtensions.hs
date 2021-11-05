module App.OptionExtensions (uriOption, jsonOption) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.String (IsString (fromString))
import Data.String.Conversion (toText)
import Options.Applicative (
  Mod,
  OptionFields,
  Parser,
  ReadM,
  eitherReader,
  maybeReader,
  option,
 )
import Text.URI (URI, mkURI)

uriOption :: Mod OptionFields URI -> Parser URI
uriOption = option parseUri
  where
    parseUri :: ReadM URI
    parseUri = maybeReader (mkURI . toText)

jsonOption :: FromJSON a => Mod OptionFields a -> Parser a
jsonOption = option (eitherReader (eitherDecode . fromString))
