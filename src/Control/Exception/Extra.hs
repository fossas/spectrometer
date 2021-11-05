module Control.Exception.Extra (
  isSyncException,
  safeCatch,
) where

import Control.Effect.Exception (
  Exception (fromException, toException),
  Has,
  Lift,
  SomeAsyncException (SomeAsyncException),
  catch,
  throwIO,
 )

safeCatch :: (Exception e, Has (Lift IO) sig m) => m a -> (e -> m a) -> m a
safeCatch act hdl = act `catch` (\e -> if isSyncException e then hdl e else throwIO e)

isSyncException :: Exception e => e -> Bool
isSyncException e =
  case fromException (toException e) of
    Just (SomeAsyncException _) -> False
    Nothing -> True
