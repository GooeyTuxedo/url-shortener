module Main where

import App (initConnectionPool, runMigrations, startApp)
import Config (loadConfig)

main :: IO ()
main = do
    -- Load application configuration from environment variables
    config <- loadConfig
    
    -- Initialize database connection pool
    pool <- initConnectionPool (dbConfig config)
    
    -- Run database migrations
    runMigrations pool
    
    -- Start the web server
    startApp config pool