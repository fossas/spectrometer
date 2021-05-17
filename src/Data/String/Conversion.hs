{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Inspiration taken from Relude. Convenient string conversion functions
module Data.String.Conversion (
  ConvertUtf8 (..),
  ToText (..),
  ToLText (..),
  ToString (..),
  LazyStrict (..),
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import GHC.TypeLits

----- ConvertUtf8

class ConvertUtf8 a b where
  encodeUtf8 :: a -> b
  decodeUtf8 :: b -> a

instance ConvertUtf8 String BS.ByteString where
  encodeUtf8 = TE.encodeUtf8 . T.pack
  decodeUtf8 = T.unpack . TE.decodeUtf8With TE.lenientDecode

instance ConvertUtf8 String BL.ByteString where
  encodeUtf8 = TLE.encodeUtf8 . TL.pack
  decodeUtf8 = TL.unpack . TLE.decodeUtf8With TE.lenientDecode

instance ConvertUtf8 T.Text BS.ByteString where
  encodeUtf8 = TE.encodeUtf8
  decodeUtf8 = TE.decodeUtf8With TE.lenientDecode

instance ConvertUtf8 T.Text BL.ByteString where
  encodeUtf8 = BL.fromStrict . TE.encodeUtf8
  decodeUtf8 = TE.decodeUtf8With TE.lenientDecode . BL.toStrict

instance ConvertUtf8 TL.Text BS.ByteString where
  encodeUtf8 = BL.toStrict . TLE.encodeUtf8
  decodeUtf8 = TLE.decodeUtf8With TE.lenientDecode . BL.fromStrict

instance ConvertUtf8 TL.Text BL.ByteString where
  encodeUtf8 = TLE.encodeUtf8
  decodeUtf8 = TLE.decodeUtf8With TE.lenientDecode

----- ToText

class ToText a where
  toText :: a -> T.Text

instance ToText String where
  toText = T.pack

instance ToText T.Text where
  toText = id

instance ToText TL.Text where
  toText = TL.toStrict

instance TypeError ( 'Text "Error: Use encodeUtf8/decodeUtf8 instead") => ToText BS.ByteString where
  toText = error "unreachable"

instance TypeError ( 'Text "Error: Use encodeUtf8/decodeUtf8 instead") => ToText BL.ByteString where
  toText = error "unreachable"

----- ToLText

class ToLText a where
  toLText :: a -> TL.Text

instance ToLText String where
  toLText = TL.pack

instance ToLText T.Text where
  toLText = TL.fromStrict

instance ToLText TL.Text where
  toLText = id

instance TypeError ( 'Text "Error: Use encodeUtf8/decodeUtf8 instead") => ToLText BS.ByteString where
  toLText = error "unreachable"

instance TypeError ( 'Text "Error: Use encodeUtf8/decodeUtf8 instead") => ToLText BL.ByteString where
  toLText = error "unreachable"

----- ToString

class ToString a where
  toString :: a -> String

instance ToString T.Text where
  toString = T.unpack

instance ToString TL.Text where
  toString = TL.unpack

instance TypeError ( 'Text "Error: Use decodeUtf8 instead") => ToString BS.ByteString where
  toString = error "unreachable"

instance TypeError ( 'Text "Error: Use decodeUtf8 instead") => ToString BL.ByteString where
  toString = error "unreachable"

----- LazyStrict

class LazyStrict l s | l -> s, s -> l where
  toLazy :: s -> l
  toStrict :: l -> s

instance LazyStrict BL.ByteString BS.ByteString where
  toLazy = BL.fromStrict
  toStrict = BL.toStrict

instance LazyStrict TL.Text T.Text where
  toLazy = TL.fromStrict
  toStrict = TL.toStrict
