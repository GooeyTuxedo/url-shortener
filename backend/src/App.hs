{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App
    ( startApp
    , initConnectionPool
    , runMigrations
    ) where

import Config (AppConfig(..), DatabaseConfig(..))
import Control.Monad.Logger (NoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (ConnectionString, SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Models (migrateAll)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Middleware (securityHeaders, rateLimitMiddleware)

import Api (apiHandler)
import AppEnv (AppEnv(..))
import AbuseProtection (BlacklistConfig(..), UrlContentFilter, newUrlContentFilter)
import RateLimiter (RateLimiter, RateLimitConfig(..), newRateLimiter)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Set as Set

-- Initialize a PostgreSQL connection pool
initConnectionPool :: DatabaseConfig -> IO (Pool SqlBackend)
initConnectionPool dbConfig = do
    let connStr = createConnectionString dbConfig
    runStdoutLoggingT $ createPostgresqlPool connStr 10

-- Create a PostgreSQL connection string from config
createConnectionString :: DatabaseConfig -> ConnectionString
createConnectionString DatabaseConfig{..} =
    encodeUtf8 $ T.concat 
        [ "host=", dbHost
        , " port=", T.pack (show dbPort)
        , " user=", dbUser
        , " password=", dbPassword
        , " dbname=", dbName
        ]

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
            { maxRequests = rateLimitRequests
            , perTimeWindow = rateLimitWindow
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
            , maxUrlLength = maxUrlLength
            }
    contentFilter <- newUrlContentFilter blacklistConfig
    
    -- Create HTTP manager
    httpManager <- newManager tlsManagerSettings
    
    -- Create application environment
    let env = AppEnv
            { envConfig = config
            , envPool = pool
            , envRateLimiter = rateLimiter
            , envContentFilter = contentFilter
            , envHttpManager = httpManager
            }
    
    -- Run the web server with middleware
    let middleware = logStdoutDev . simpleCors . securityHeaders . rateLimitMiddleware rateLimiter
    run appPort $ middleware $ apiHandler env
