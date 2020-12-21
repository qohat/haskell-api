module Application.Users (UserService (..)) where

import qualified Domain.User as DOMAIN
import Data.Text (Text)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class ()

class (Monad m) => UserService m where
  findById :: 
    (Text -> m (Maybe DOMAIN.User)) ->
    Text -> 
    m (Maybe DOMAIN.User)
  findAll :: m [DOMAIN.User] -> m [DOMAIN.User]
  save :: (DOMAIN.User -> m Bool) -> DOMAIN.User -> m ()
  deleteById :: (Text -> m Bool) -> Text -> m ()
  update :: 
    (Text -> Text -> Int -> Text -> m Bool) ->
    Text ->
    Text ->
    Int ->
    Text ->
    m ()
    
instance UserService IO where
  findById f id' = f id'
  findAll f = f
  save f user' = save' f user'
  deleteById = deleteById'
  update f id' name age email = 
    update' f id' name age email
    
save' ::
  MonadThrow m =>
  (DOMAIN.User -> m Bool) ->
  DOMAIN.User ->
  m ()

save' f' u = do
  result <- f' u
  if result
    then return ()
    else throwM $ DOMAIN.UserException "There are no results"
    
deleteById' ::
  MonadThrow m =>
  (Text -> m Bool) ->
  Text ->
  m ()
  
deleteById' f' id' = do
  result <- f' id'
  if result
    then return ()
    else throwM $ DOMAIN.UserException "There are no results"
    
update' ::
  MonadThrow m =>
  (Text -> Text -> Int -> Text -> m Bool) ->
  Text ->
  Text ->
  Int ->
  Text ->
  m ()
  
update' f' id' name' age' email' = do
  result <- f' id' name' age' email'
  if result
    then return ()
    else throwM $ DOMAIN.UserException "There are no results"
    
