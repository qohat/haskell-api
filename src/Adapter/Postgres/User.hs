{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}


module Adapter.Postgres.User (UserRepository (..)) where

import Data.Text (Text)
import qualified Domain.User as DOMAIN
import qualified Database.PostgreSQL.Simple as PG
import qualified Adapter.Postgres.Util as UTIL
import Data.Pool (Pool)
import qualified Adapter.Async.AsyncRunner as ASYNC
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (ZonedTime)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Control.Exception (catch)
import Control.Monad.Catch (MonadThrow, throwM)

class (Monad m, Functor m) => UserRepository m c where
  findById :: c -> Text -> m (Maybe DOMAIN.User)
  findAll :: c -> m [DOMAIN.User]
  save :: c -> DOMAIN.User -> m Bool
  deleteById :: c -> Text -> m Bool
  update :: c -> Text -> Text -> Int -> Text -> m Bool
  
instance UserRepository IO (Pool PG.Connection) where
  findById pool id' = do
    result <- ASYNC.run $ findById' pool id'
    case result of
      Just user -> return $ Just (to user)
      Nothing -> return Nothing
  findAll pool = ASYNC.run $ fmap (map to) (findAll' pool)
  save pool user' = ASYNC.run $ save' pool user'
  deleteById pool id' = deleteById' pool id'
  update pool id' name age email = update' pool id' name age email

data UserRow = UserRow
  { _id :: Text,
    _name :: Text,
    _age :: Int,
    _email :: Text,
    _created_at :: ZonedTime
  }
  
instance FromRow UserRow where
  fromRow = UserRow <$> field <*> field <*> field <*> field <*> field
  
instance ToRow UserRow where
  toRow p =
    [ toField (_id p),
      toField (_name p),
      toField (_age p),
      toField (_email p),
      toField (_created_at p)
    ]

findById' :: 
  MonadIO m =>
  Pool PG.Connection ->
  Text ->
  m (Maybe UserRow)
findById' pool' id' =
  liftIO $
    UTIL.queryRow pool' sql [id' :: Text]
      `catch` handlePgException
    where
      sql =
        "SELECT * FROM users\n\
        \WHERE id = ?"

findAll' ::
  MonadIO m =>
  Pool PG.Connection ->
  m [UserRow]
findAll' pool' =
  liftIO $
    UTIL.queryRows pool' sql
      `catch` handlePgException
  where
    sql = "SELECT * FROM users"

save' ::
  MonadIO m =>
  Pool PG.Connection ->
  DOMAIN.User ->
  m Bool
save' pool' user' = do
  let row = from user'
  liftIO $
    UTIL.command pool' sql (_id row, _name row, _age row, _email row, _created_at row)
      `catch` handlePgException
  where
    sql =
      "INSERT INTO users (id, name, age, email, created_at)\n\
      \VALUES (?, ?, ?, ?, ?)"

deleteById' ::
  MonadIO m =>
  Pool PG.Connection ->
  Text ->
  m Bool
deleteById' pool' id' = do
  liftIO $
    UTIL.command pool' sql (PG.Only (id' :: Text))
      `catch` handlePgException
  where
    sql =
      "DELETE FROM users\n\
      \WHERE id = ?"

update' ::
  MonadIO m =>
  Pool PG.Connection ->
  Text ->
  Text ->
  Int ->
  Text ->
  m Bool
update' pool' id' name' age' email' = do
  liftIO $
    UTIL.command pool' sql (name' :: Text, age' :: Int, email' :: Text, id' :: Text)
      `catch` handlePgException
  where
    sql =
      "UPDATE users\n\
      \SET name = ?, age = ?, email = ?\n\
      \WHERE id = ?"

handlePgException :: MonadThrow m => UTIL.PostgresException -> m a
handlePgException (UTIL.PostgresException em) = throwM $ DOMAIN.UserException em

to :: UserRow -> DOMAIN.User
to row =
  DOMAIN.User
    { DOMAIN.userId = DOMAIN.UserId (_id row),
      DOMAIN.userName = DOMAIN.UserName (_name row),
      DOMAIN.userAge = DOMAIN.UserAge (_age row),
      DOMAIN.userEmail = DOMAIN.UserEmail (_email row),
      DOMAIN.userCreatedAt = DOMAIN.UserCreatedAt (_created_at row)
    }

from :: DOMAIN.User -> UserRow
from p =
  UserRow
    { _id = DOMAIN.id (DOMAIN.userId p),
      _name = DOMAIN.name (DOMAIN.userName p),
      _age = DOMAIN.age (DOMAIN.userAge p),
      _email = DOMAIN.email (DOMAIN.userEmail p),
      _created_at = DOMAIN.createdAt (DOMAIN.userCreatedAt p)
    }
    
    