{-# LANGUAGE DeriveDataTypeable #-}

module Domain.User where

import Data.Text (Text)
import Data.Time (ZonedTime)
import Data.Typeable (Typeable)
import Control.Exception (Exception)


newtype UserId = UserId { id :: Text } deriving (Show, Eq)
newtype UserName = UserName { name :: Text} deriving (Show, Eq)
newtype UserAge = UserAge { age :: Int } deriving (Show, Eq)
newtype UserEmail = UserEmail { email :: Text } deriving (Show, Eq)
newtype UserCreatedAt = UserCreatedAt { createdAt :: ZonedTime } deriving (Show)

data User = User 
  {
    userId :: UserId,
    userName :: UserName,
    userAge :: UserAge,
    userEmail :: UserEmail,
    userCreatedAt :: UserCreatedAt
  }
  deriving (Show)
  
instance Eq User where
  (==) u1 u2 = userId u1 == userId u2
  (/=) u1 u2 = userId u1 /= userId u2
  
newtype UserException = UserException String deriving (Show, Eq, Typeable)

instance Exception UserException