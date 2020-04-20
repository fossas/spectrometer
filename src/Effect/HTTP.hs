module Effect.HTTP
(
  HTTPErr(..)
) where

import Prologue

data HTTPErr =
    NoUrlError Text -- ^ No Url provided. err
  deriving (Eq, Ord, Show, Generic, Typeable)