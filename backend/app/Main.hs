module Main where

import Api (apiHandler)
import AppEnv (AppEnv(..))
import App (initConnectionPool, runMigrations)
import Config (loadConfig)
import qualified Config as C
import Control.Exception (bracket)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Middleware (optionsMiddleware, securityHeaders, rateLimitMiddleware)
import AbuseProtection (newUrlContentFilter)
import qualified AbuseProtection as A
import RateLimiter (RateLimiter, RateLimitConfig(..), newRateLimiter)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Method (Method, methodGet, methodPost, methodPut, methodDelete, methodOptions)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
    -- Load application configuration
    config <- loadConfig
    
    -- Initialize resources with proper cleanup
    bracket initResources cleanupResources $ \env -> do
        -- Run the web server
        let port = C.appPort (envConfig env)
        putStrLn $ "Starting server on port " ++ show port
        run port $ middleware env $ apiHandler env
  where
    -- Initialize all application resources
    initResources = do
        config <- loadConfig
        
        -- Initialize database
        pool <- initConnectionPool (C.dbConfig config)
        runMigrations pool
        
        -- Initialize rate limiter
        let rateLimitConfig = RateLimitConfig
                { maxRequests = C.rateLimitRequests config
                , perTimeWindow = C.rateLimitWindow config
                , cleanupInterval = 300  -- clean up every 5 minutes
                }
        rateLimiter <- newRateLimiter rateLimitConfig
        
        -- Initialize content filter
        let blacklistConfig = A.BlacklistConfig
                { A.blacklistDomains = Set.fromList 
                    [ T.pack "malware-site.com", T.pack "phishing-example.com" ]
                , A.blacklistPatterns = Set.fromList 
                    [ T.pack "phish", T.pack "malware", T.pack "exploit", T.pack "hack" ]
                , A.blacklistFile = Nothing
                , A.maxRedirects = 5
                , A.maxUrlLength = C.maxUrlLength config
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
        optionsMiddleware . 
        logStdoutDev . 
        securityHeaders . 
        rateLimitMiddleware (envRateLimiter env)
