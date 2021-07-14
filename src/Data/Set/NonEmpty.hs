module Data.Set.NonEmpty
  ( NonEmptySet
  , nonEmpty
  , toSet
  )
  where

import qualified Data.Set as S

nonEmpty :: S.Set a -> Maybe (NonEmptySet a)
nonEmpty s
  | S.null s = Nothing
  | otherwise = Just (NonEmptySet s)

toSet :: NonEmptySet a -> S.Set a
toSet = unEmptySet

newtype NonEmptySet a = NonEmptySet { unEmptySet :: S.Set a } deriving (Eq, Ord, Show)