module Data.Text.Extra
  ( splitOnceOn,
    splitOnceOnEnd,
    strippedPrefix,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

strippedPrefix :: Text -> Text -> Text
strippedPrefix pre txt = maybe txt id (T.stripPrefix pre txt)

splitOnceOn :: Text -> Text -> (Text, Text)
splitOnceOn needle haystack = (first, strippedRemaining)
  where
    len = T.length needle
    (first, remaining) = T.breakOn needle haystack
    strippedRemaining = T.drop len remaining

splitOnceOnEnd :: Text -> Text -> (Text, Text)
splitOnceOnEnd needle haystack = (strippedInitial, end)
  where
    len = T.length needle
    (initial, end) = T.breakOnEnd needle haystack
    strippedInitial = T.dropEnd len initial
