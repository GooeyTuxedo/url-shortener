module Main where

import Api (apiHandler)
import AppEnv (AppEnv(..))
import App (initConnectionPool, runMigrations)
import Config (loadConfig)
import Control.Exception (bracket)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Middleware (securityHeaders, rateLimitMiddleware)
import AbuseProtection (BlacklistConfig(..), newUrlContentFilter)
import RateLimiter (RateLimiter, RateLimitConfig(..), newRateLimiter)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Set as Set

main :: IO ()
main = do
    -- Load application configuration
    config <- loadConfig
    
    -- Initialize resources with proper cleanup
    bracket initResources cleanupResources $ \env -> do
        -- Run the web server
        let port = appPort (envConfig env)
        putStrLn $ "Starting server on port " ++ show port
        run port $ middleware env $ apiHandler env
  where
    -- Initialize all application resources
    initResources = do
        config <- loadConfig
        
        -- Initialize database
        pool <- initConnectionPool (dbConfig config)
        runMigrations pool
        
        -- Initialize rate limiter
        let rateLimitConfig = RateLimitConfig
                { maxRequests = rateLimitRequests config
                , perTimeWindow = rateLimitWindow config
                , cleanupInterval = 300  -- clean up every 5 minutes
                }
        rateLimiter <- newRateLimiter rateLimitConfig
        
        -- Initialize content filter
        let blacklistConfig = BlacklistConfig
                { blacklistDomains = Set.fromList 
                    [ "malware-site.com", "phishing-example.com" ]
                , blacklistPatterns = Set.fromList 
                    [ "phish", "malware", "exploit", "hack" ]
                , blacklistFile = Nothing
                , maxRedirects = 5
                , maxUrlLength = maxUrlLength config
                }
        contentFilter <- newUrlContentFilter blacklistConfig
        
        -- Initialize HTTP manager
        httpManager <- newManager tlsManagerSettings
        
        -- Return the complete environment
        return AppEnv
            { envConfig = config
            , envPool = pool
            , envRateLimiter = rateLimiter
            , envContentFilter = contentFilter
            , envHttpManager = httpManager
            }
    
    -- Clean up all resources
    cleanupResources env = do
        -- Add resource cleanup here
        -- For example: close database connections, stop background threads
        putStrLn "Shutting down server and cleaning up resources..."
        return ()
    
    -- Apply middleware to the application
    middleware env = 
        logStdoutDev . 
        simpleCors . 
        securityHeaders . 
        rateLimitMiddleware (envRateLimiter env)
