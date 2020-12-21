{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Adapter.Http.UserRouting as HTTP
import Adapter.Logger.Logger (logger)
import qualified Adapter.Postgres.Config as DB
import qualified Adapter.Postgres.Migration as M
import qualified Adapter.Postgres.User as REPO
import qualified Application.Users as SERVICE
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Katip (Severity (InfoS), logTM, runKatipContextT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( cors,
    corsMethods,
    corsRequestHeaders,
    simpleCorsResourcePolicy,
  )
import Servant (serve)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  pool <- DB.create
  dir <- lookupEnv "DB_MIGRATION_DIR"
  port' <- 
    lookupEnv "APP_PORT"
        >>= \case
            Just p -> return (read p :: Int)
            Nothing -> return 8080
  _ <- M.migrate pool (fromMaybe "db/migrations" dir)
  let f1 = SERVICE.findAll (REPO.findAll pool)
      f2 = SERVICE.findById (REPO.findById pool)
      f3 = SERVICE.save (REPO.save pool)
      f4 = SERVICE.deleteById (REPO.deleteById pool)
      f5 = SERVICE.update (REPO.update pool)
      server = serve proxy $ routes f1 f2 f3 f4 f5
  logger $ \logEnv -> do
    runKatipContextT logEnv () "server-start" $ do
      $(logTM) InfoS "Start Server..."
  run port' $ corsMiddleware server -- run port' $ simpleCors server

type API = HTTP.UserRoute

routes = HTTP.routes

proxy :: Proxy API
proxy = Proxy

corsMiddleware :: Application -> Application
corsMiddleware =
  cors
    ( const $
        Just
          ( simpleCorsResourcePolicy
              { corsMethods = ["DELETE", "GET", "OPTIONS", "PATCH", "POST", "PUT"],
                corsRequestHeaders = ["Content-Type"]
              }
          )
    )

--server :: Application