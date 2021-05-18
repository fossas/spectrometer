
{-# LANGUAGE GADTs #-}

module Control.Effect.Fresh
  ( Fresh(..)
  , generateId
  , module X
  ) where

import Control.Algebra as X

data Fresh m a where
  GenerateId :: Fresh m Int

generateId :: Has Fresh sig m => m Int
generateId = send GenerateId
