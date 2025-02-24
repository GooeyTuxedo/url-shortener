{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config
    ( AppConfig(..)
    , DatabaseConfig(..)
    , loadConfig
    ) where

import Control.Monad (liftM2)
import Data.Text (Text)
import qualified Data.Text as T
import Envparse
import Network.Wai.Handler.Warp (Port)

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
    , rateLimitRequests :: Int       -- Requests per window
    , rateLimitWindow :: Int         -- Time window in seconds
    , maxUrlLength :: Int            -- Maximum allowed URL length
    , enableAbusePrevention :: Bool  -- Enable/disable abuse prevention
    } deriving (Show)

loadConfig :: IO AppConfig
loadConfig = envparse parser

parser :: Parser AppConfig
parser = do
    appPort <- envInt "PORT" (Var "Application port" (Just 8080))
    
    dbHost <- envText "DB_HOST" (Var "Database host" (Just "localhost"))
    dbPort <- envInt "DB_PORT" (Var "Database port" (Just 5432))
    dbUser <- envText "DB_USER" (Var "Database user" (Just "postgres"))
    dbPassword <- envText "DB_PASSWORD" (Var "Database password" (Just "postgres"))
    dbName <- envText "DB_NAME" (Var "Database name" (Just "urlshortener"))
    
    baseUrl <- envText "BASE_URL" (Var "Base URL for shortened links" (Just "http://localhost:8080"))
    shortCodeLength <- envInt "SHORT_CODE_LENGTH" (Var "Length of generated short codes" (Just 6))
    
    -- Rate limiting and abuse prevention configuration
    rateLimitRequests <- envInt "RATE_LIMIT_REQUESTS" (Var "Maximum requests per time window" (Just 100))
    rateLimitWindow <- envInt "RATE_LIMIT_WINDOW" (Var "Rate limit time window in seconds" (Just 60))
    maxUrlLength <- envInt "MAX_URL_LENGTH" (Var "Maximum URL length allowed" (Just 2048))
    enableAbusePrevention <- envBool "ENABLE_ABUSE_PREVENTION" (Var "Enable abuse prevention features" (Just True))
    
    let dbConfig = DatabaseConfig {..}
    
    return AppConfig {..}
  where
    envBool :: String -> EnvF String -> Parser Bool
    envBool var desc = do
      val <- env var desc
      return $ case val of
        "true"  -> True
        "True"  -> True
        "1"     -> True
        _       -> False

envInt :: String -> EnvF String -> Parser Int
envInt var desc = liftM2 fromIntegral (env var desc :: Parser Int) (return 1)

envText :: String -> EnvF String -> Parser Text
envText var desc = liftM2 T.pack (env var desc) (return "")