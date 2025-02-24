{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config
    ( AppConfig(..)
    , DatabaseConfig(..)
    , loadConfig
    ) where

import Control.Exception (try)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai.Handler.Warp (Port)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data DatabaseConfig = DatabaseConfig
    { dbHost :: Text
    , dbPort :: Int
    , dbUser :: Text
    , dbPassword :: Text
    , dbName :: Text
    } deriving (Show)

data AppConfig = AppConfig
    { appPort :: Port
    , dbConfig :: DatabaseConfig
    , baseUrl :: Text
    , shortCodeLength :: Int
    , rateLimitRequests :: Int
    , rateLimitWindow :: Int
    , maxUrlLength :: Int
    , enableAbusePrevention :: Bool
    } deriving (Show)

-- Load configuration from environment variables
loadConfig :: IO AppConfig
loadConfig = do
    -- App configuration
    appPort <- getEnvInt "PORT" 8080
    
    -- Database configuration
    dbHost <- getEnvText "DB_HOST" "localhost"
    dbPort <- getEnvInt "DB_PORT" 5432
    dbUser <- getEnvText "DB_USER" "postgres"
    dbPassword <- getEnvText "DB_PASSWORD" "postgres"
    dbName <- getEnvText "DB_NAME" "urlshortener"
    
    -- URL shortener configuration
    baseUrl <- getEnvText "BASE_URL" "http://localhost:8080"
    shortCodeLength <- getEnvInt "SHORT_CODE_LENGTH" 6
    
    -- Rate limiting configuration
    rateLimitRequests <- getEnvInt "RATE_LIMIT_REQUESTS" 100
    rateLimitWindow <- getEnvInt "RATE_LIMIT_WINDOW" 60
    maxUrlLength <- getEnvInt "MAX_URL_LENGTH" 2048
    enableAbusePrevention <- getEnvBool "ENABLE_ABUSE_PREVENTION" True
    
    let dbConfig = DatabaseConfig {..}
    
    return AppConfig {..}

-- Helper functions to get environment variables with default values
getEnvText :: String -> String -> IO Text
getEnvText name defaultValue = do
    maybeValue <- lookupEnv name
    return $ T.pack $ fromMaybe defaultValue maybeValue

getEnvInt :: String -> Int -> IO Int
getEnvInt name defaultValue = do
    maybeValue <- lookupEnv name
    return $ fromMaybe defaultValue (maybeValue >>= readMaybe)

getEnvBool :: String -> Bool -> IO Bool
getEnvBool name defaultValue = do
    maybeValue <- lookupEnv name
    return $ case maybeValue of
        Just "true" -> True
        Just "True" -> True
        Just "1" -> True
        Just "false" -> False
        Just "False" -> False
        Just "0" -> False
        _ -> defaultValue
