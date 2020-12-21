module Adapter.Async.AsyncRunner where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Concurrent.Async (async, waitCatch)


class (Monad m, MonadThrow m) => AsyncRunner m where
  run :: m a -> m a
  
instance AsyncRunner IO where
  run action = do
    val <- async action
    result <- waitCatch val
    case result of
      Right v -> return v
      Left e -> throwM e