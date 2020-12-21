{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Adapter.Http.UserRouting where

import Adapter.Http.UserCommand as COMMAND (UserCommandRoute, routes)
import Adapter.Http.UserQuery as QUERY (UserQueryRoute, routes)
import Data.Text (Text)
import Domain.User (User)
import Servant (Server, type (:<|>) (..), type (:>))

type UserRoute =
  "users" :> (QUERY.UserQueryRoute :<|> COMMAND.UserCommandRoute)
  
routes ::
  IO [User] ->
  (Text -> IO (Maybe User)) ->
  (User -> IO ()) ->
  (Text -> IO ()) ->
  (Text -> Text -> Int -> Text -> IO ()) ->
  Server UserRoute
  
routes f1 f2 f3 f4 f5 = QUERY.routes f1 f2 :<|> COMMAND.routes f3 f4 f5
  
