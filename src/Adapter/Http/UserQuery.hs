{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Adapter.Http.UserQuery(UserQueryRoute, routes) where

import Application.Users ()
import Control.Monad.IO.Class (liftIO)
import qualified Domain.User as DOMAIN
import Data.Aeson (ToJSON)
import Servant 
  ( JSON,
    Get,
    Capture,
    type (:<|>) (..),
    type (:>), 
    Server, 
    Handler, 
    errBody, 
    throwError, 
    err404, 
    err500,
  )
import Data.Text (Text)
import Data.Time (ZonedTime)
import GHC.Generics (Generic)
import Control.Exception (try)


type UserQueryRoute =
  ( Get '[JSON] [UserQueryDto]
      :<|> Capture "id" Text :> Get '[JSON] UserQueryDto
  )
  
data UserQueryDto = UserQueryDto
  { user_id :: Text,
    user_name :: Text,
    user_age :: Int,
    user_email :: Text,
    user_created_at :: ZonedTime
  }
  deriving (Generic, Show)
  
instance ToJSON UserQueryDto

routes :: 
  IO [DOMAIN.User] ->
  (Text -> IO (Maybe DOMAIN.User)) ->
  Server UserQueryRoute
routes f1 f2 = allUsers f1 :<|> oneUser f2

oneUser :: (Text -> IO (Maybe DOMAIN.User)) -> Text -> Handler UserQueryDto
oneUser f' id' = do
  result <- liftIO $ f' id'
  case result of
    Just value -> return $ to value
    Nothing ->
      throwError
        err404
          { errBody = "User not found"
          }
          
allUsers :: IO [DOMAIN.User] -> Handler [UserQueryDto]
allUsers f' = do
  result <- liftIO $ try (fmap (map to) f')
  case result of
    Right v -> return v
    Left e ->
      case e of
        DOMAIN.UserException _ ->
          throwError
            err500
              { errBody = "Error getting all users"
              }

to :: DOMAIN.User -> UserQueryDto
to u =
  UserQueryDto
    { user_id = DOMAIN.id (DOMAIN.userId u),
      user_name = DOMAIN.name (DOMAIN.userName u),
      user_age = DOMAIN.age (DOMAIN.userAge u),
      user_email = DOMAIN.email (DOMAIN.userEmail u),
      user_created_at = DOMAIN.createdAt (DOMAIN.userCreatedAt u)
    }