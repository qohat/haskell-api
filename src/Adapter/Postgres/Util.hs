{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Adapter.Postgres.Util
  ( Postgres (..),
    PostgresException (..),
  ) 
where

import Adapter.Logger.Logger (logger)
import Control.Exception (Exception, catch)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool (Pool, withResource)
import Data.Typeable (Typeable)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Katip
  ( Namespace,
    Severity (ErrorS, InfoS),
    logStr,
    logTM,
    runKatipContextT,
  )

class Postgres m where
  queryRow ::
    (FromRow a, ToRow b) =>
    Pool PG.Connection ->
    PG.Query ->
    b ->
    m (Maybe a)
  queryRows ::
    FromRow a =>
    Pool PG.Connection ->
    PG.Query ->
    m [a]
  command ::
    (ToRow b) =>
    Pool PG.Connection ->
    PG.Query ->
    b ->
    m Bool
    
instance Postgres IO where
  queryRow pool q b = queryRow' pool q b
  queryRows pool q = queryRows' pool q
  command pool c b = command' pool c b
  
queryRow' ::
  (MonadIO m, FromRow a, ToRow b) =>
  Pool PG.Connection ->
  PG.Query ->
  b ->
  m (Maybe a)
queryRow' p' q' b = do
  liftIO $
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS "Query One Start..."
  result <-
    liftIO $
      withResource p' (\conn -> PG.query conn q' b)
        `catch` \e1 ->
          handleSqlError e1 namespace
            `catch` \e2 ->
              handleResultError e2 namespace
                `catch` \e3 ->
                  handleFormatError e3 namespace
                    `catch` \e4 -> 
                      handleQueryError e4 namespace
  liftIO $
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS "Query One End..."
  case result of
    (h : _) -> return $ Just h
    _ -> return Nothing
  where
    namespace = "query-one"

queryRows' ::
  (MonadIO m, FromRow a) =>
  Pool PG.Connection ->
  PG.Query ->
  m [a]
queryRows' p' q' = do
  liftIO $
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS "Query List Without Params Start..."
  result <-
    liftIO $
      withResource p' (`PG.query_` q')
        `catch` \e1 ->
          handleSqlError e1 namespace
            `catch` \e2 ->
              handleResultError e2 namespace
                `catch` \e3 ->
                  handleFormatError e3 namespace
                    `catch` \e4 -> handleQueryError e4 namespace
  liftIO $
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS "Query List Without Params End..."
  return result
  where
    namespace = "query-list-without-params"

    
command' ::
  (MonadIO m, ToRow b) =>
  Pool PG.Connection ->
  PG.Query ->
  b ->
  m Bool
command' p' q' b = do
  liftIO $
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS "Command Start..."
  result <-
    liftIO $
      withResource p' (\conn -> PG.execute conn q' b)
        `catch` \e1 ->
          handleSqlError e1 namespace
            `catch` \e2 -> handleFormatError e2 namespace
  liftIO $
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) InfoS "Command End..."
  return (result > 0)
  where
    namespace = "command"

newtype PostgresException = PostgresException String deriving (Show, Typeable)

instance Exception PostgresException
  
handleSqlError ::
  (MonadIO m, MonadThrow m) =>
  PG.SqlError ->
  Namespace ->
  m a
handleSqlError e@(PG.SqlError _ _ msg _ _) namespace = do
  liftIO $
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) ErrorS $ logStr ("SQL Error: " <> show e)
  throwM $ PostgresException (show e)

handleResultError ::
  (MonadIO m, MonadThrow m) =>
  PG.ResultError ->
  Namespace ->
  m a
handleResultError e namespace = do
  liftIO $
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) ErrorS $ logStr ("SQL Result Error: " <> show e)
  case e of
    PG.Incompatible _ _ _ _ msg -> throwM $ PostgresException msg
    PG.UnexpectedNull _ _ _ _ msg -> throwM $ PostgresException msg
    PG.ConversionFailed _ _ _ _ msg -> throwM $ PostgresException msg

handleFormatError ::
  (MonadIO m, MonadThrow m) =>
  PG.FormatError ->
  Namespace ->
  m a
handleFormatError e namespace = do
  liftIO $
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) ErrorS $ logStr ("SQL Format Error: " <> show e)
  throwM $ PostgresException (show e)

handleQueryError ::
  (MonadIO m, MonadThrow m) =>
  PG.QueryError ->
  Namespace ->
  m a
handleQueryError e namespace = do
  liftIO $
    logger $ \logEnv -> do
      runKatipContextT logEnv () namespace $ do
        $(logTM) ErrorS $ logStr ("SQL Query Error: " <> show e)
  throwM $ PostgresException (show e)
    
    