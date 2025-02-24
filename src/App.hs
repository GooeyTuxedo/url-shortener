{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App
    ( AppState(..)
    , startApp
    , initConnectionPool
    , runMigrations
    ) where

import Config (AppConfig(..), DatabaseConfig(..))
import Control.Monad.Logger (NoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql (ConnectionString, SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Models (migrateAll)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Application)

import Api (app)

-- Application state containing all resources needed at runtime
data AppState = AppState
    { appStatePool :: Pool SqlBackend
    , appStateConfig :: AppConfig
    }

-- Initialize a PostgreSQL connection pool
initConnectionPool :: DatabaseConfig -> IO (Pool SqlBackend)
initConnectionPool dbConfig = do
    let connStr = createConnectionString dbConfig
    runStdoutLoggingT $ createPostgresqlPool connStr 10

-- Create a PostgreSQL connection string from config
createConnectionString :: DatabaseConfig -> ConnectionString
createConnectionString DatabaseConfig{..} =
    "host=" <> (T.unpack dbHost) <>
    " port=" <> show dbPort <>
    " user=" <> (T.unpack dbUser) <>
    " password=" <> (T.unpack dbPassword) <>
    " dbname=" <> (T.unpack dbName)

-- Run database migrations
runMigrations :: Pool SqlBackend -> IO ()
runMigrations pool = do
    runSqlPool (runMigration migrateAll) pool

-- Start the web server with the given configuration
startApp :: AppConfig -> Pool SqlBackend -> IO ()
startApp config@AppConfig{..} pool = do
    putStrLn $ "Starting server on port " ++ show appPort
    putStrLn $ "Base URL set to: " ++ T.unpack baseUrl
    
    -- Create application state
    let state = AppState
            { appStatePool = pool
            , appStateConfig = config
            }
    
    -- Run the web server
    run appPort $ logStdoutDev $ simpleCors $ app state