{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Adapter.Http.UserCommand (UserCommandRoute, routes) where

import Application.Users ()
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import qualified Domain.User as DOMAIN
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import qualified Data.UUID as UUID
import Data.UUID.V1 (nextUUID)
import Data.Time (getZonedTime, ZonedTime)
import Data.Maybe (fromJust)
import Servant 
  ( ReqBody, 
    JSON, 
    PostCreated, 
    NoContent (..), 
    Capture, 
    Delete,
    Put, 
    Server,
    Handler,
    Server,
    ServerError (errBody),
    errBody, 
    err400, 
    throwError,
    type (:<|>) (..), 
    type (:>) (..),
  )

type UserCommandRoute =
  ( ReqBody '[JSON] UserCommandDto :> PostCreated '[JSON] NoContent
    :<|> Capture "id" Text :> Delete '[JSON] NoContent
    :<|> Capture "id" Text :> ReqBody '[JSON] UserCommandDto :> Put '[JSON] NoContent
  )
  
data UserCommandDto = UserCommandDto
  { user_name :: Text,
    user_age :: Int,
    user_email :: Text
  }
  deriving (Show, Generic)
  
instance FromJSON UserCommandDto

routes ::
  (DOMAIN.User -> IO ()) ->
  (Text -> IO ()) ->
  (Text -> Text -> Int -> Text -> IO ()) ->
  Server UserCommandRoute
routes f1 f2 f3 = saveUser f1 :<|> deleteUser f2 :<|> updateUser f3

saveUser ::
  (DOMAIN.User -> IO ()) ->
  UserCommandDto ->
  Handler NoContent
saveUser f' user' = do
  uuid <- liftIO nextUUID
  let uuid' = pack (UUID.toString (fromJust uuid))
  createdAt <- liftIO getZonedTime
  result <- liftIO $ try (f' (userfrom user' uuid' createdAt))
  case result of
    Right _ -> return NoContent
    Left e ->
      case e of
        DOMAIN.UserException _ ->
          throwError
            err400
              { errBody = "Error creating user"
              }
    
deleteUser :: (Text -> IO ()) -> Text -> Handler NoContent
deleteUser f' id' = do
  result <- liftIO $ try (f' id') 
  case result of
    Right _ -> return NoContent
    Left e ->
      case e of
        DOMAIN.UserException _ ->
          throwError
            err400
              { errBody = "Error deleting user "
              }
              
updateUser ::
  (Text -> Text -> Int -> Text -> IO ()) ->
  Text ->
  UserCommandDto ->
  Handler NoContent
updateUser f' id' user' = do
  let name = user_name user'
      age = user_age user'
      email = user_email user'
  result <- liftIO $ try (f' id' name age email)
  case result of
    Right _ -> return NoContent
    Left e ->
      case e of
        DOMAIN.UserException _ ->
          throwError
            err400
              { errBody = "Error updating user"
              }
              
userfrom :: UserCommandDto -> Text -> ZonedTime -> DOMAIN.User
userfrom user uuid createdAt = 
  DOMAIN.User 
    { DOMAIN.userId = DOMAIN.UserId uuid,
      DOMAIN.userName = DOMAIN.UserName (user_name user),
      DOMAIN.userAge = DOMAIN.UserAge (user_age user),
      DOMAIN.userEmail = DOMAIN.UserEmail (user_email user),
      DOMAIN.userCreatedAt = DOMAIN.UserCreatedAt createdAt
    }