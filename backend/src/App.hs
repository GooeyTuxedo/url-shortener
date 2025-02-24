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
import Middleware (securityHeaders, rateLimitMiddleware)
import Servant (Application)

import Api (app)

-- Application state containing all resources needed at runtime
import AbuseProtection (BlacklistConfig(..), UrlContentFilter, newUrlContentFilter, isUrlSafe)
import RateLimiter (RateLimiter, RateLimitConfig(..), newRateLimiter, checkRateLimit)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Set as Set

data AppState = AppState
    { appStatePool :: Pool SqlBackend
    , appStateConfig :: AppConfig
    , appStateRateLimiter :: RateLimiter
    , appStateContentFilter :: UrlContentFilter
    , appStateHttpManager :: Manager
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
    
    -- Create rate limiter with default config
    let rateLimitConfig = RateLimitConfig
            { maxRequests = 100      -- 100 requests 
            , perTimeWindow = 60     -- per minute
            , cleanupInterval = 300  -- clean up every 5 minutes
            }
    rateLimiter <- newRateLimiter rateLimitConfig
    
    -- Create URL content filter with default config
    let blacklistConfig = BlacklistConfig
            { blacklistDomains = Set.fromList 
                [ "malware-site.com", "phishing-example.com" ]
            , blacklistPatterns = Set.fromList 
                [ "phish", "malware", "exploit", "hack" ]
            , blacklistFile = Nothing
            , maxRedirects = 5
            , maxUrlLength = 2048
            }
    contentFilter <- newUrlContentFilter blacklistConfig
    
    -- Create HTTP manager
    httpManager <- newManager tlsManagerSettings
    
    -- Create application state
    let state = AppState
            { appStatePool = pool
            , appStateConfig = config
            , appStateRateLimiter = rateLimiter
            , appStateContentFilter = contentFilter
            , appStateHttpManager = httpManager
            }
    
    -- Run the web server with middleware
    let middleware = logStdoutDev . simpleCors . securityHeaders . rateLimitMiddleware rateLimiter
    run appPort $ middleware $ app state