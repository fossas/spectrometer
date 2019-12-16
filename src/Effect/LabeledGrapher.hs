{-# language TemplateHaskell #-}

-- | LabeledGrapher is a thin State wrapper effect around 'L.LabeledGraphing'
--
-- It defines @direct@, @edge@, and @label@ combinators analagous to 'L.direct',
-- 'L.edge', and 'L.label' from 'L.LabeledGraphing'
--
-- See also: 'withLabeling', which captures a common pattern when building graphs
module Effect.LabeledGrapher
  ( LabeledGrapher(..)
  , direct
  , edge
  , label

  , evalLabeledGrapher
  , runLabeledGrapher
  , withLabeling

  -- re-exports
  , L.PkgLabel
  ) where

import Prologue hiding (parent)

import Polysemy
import Polysemy.State

import qualified Graphing as G
import qualified LabeledGraphing as L

data LabeledGrapher ty m a where
  Direct :: ty -> LabeledGrapher ty m ()
  Edge :: ty -> ty -> LabeledGrapher ty m ()
  Label :: ty -> L.PkgLabel ty -> LabeledGrapher ty m ()

makeSem ''LabeledGrapher

evalLabeledGrapher :: (Ord ty, Ord (L.PkgLabel ty)) => Sem (LabeledGrapher ty ': r) a -> Sem r (L.LabeledGraphing ty)
evalLabeledGrapher = fmap fst . runLabeledGrapher

runLabeledGrapher :: (Ord ty, Ord (L.PkgLabel ty)) => Sem (LabeledGrapher ty ': r) a -> Sem r (L.LabeledGraphing ty, a)
runLabeledGrapher = runState L.empty . reinterpret
  (\case
      Direct ty -> modify (L.direct ty)
      Edge parent child -> modify (L.edge parent child)
      Label ty lbl -> modify (L.label ty lbl))

-- | A convenience function for a very common pattern: building a graph in a
-- LabeledGrapher context, and immediately transforming the result into a
-- 'G.Graphing'
withLabeling :: (Ord ty, Ord (L.PkgLabel ty), Ord res) => (ty -> Set (L.PkgLabel ty) -> res) -> Sem (LabeledGrapher ty ': r) a -> Sem r (G.Graphing res)
withLabeling f = fmap (L.unlabel f) . evalLabeledGrapher
