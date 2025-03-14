{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Control.Exception (try, SomeException)
import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Postgresql ((==.), (=.), ConnectionPool, Entity(..), SelectOpt(LimitTo), Single, SqlBackend, update, selectList, rawSql, runSqlPool)
import Database.Persist.TH
import GHC.Generics (Generic)

-- Database schema definitions
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ShortUrl
    originalUrl Text
    shortCode Text
    createdAt UTCTime
    expiresAt UTCTime Maybe
    clickCount Int default=0
    clientId Text
    UniqueShortCode shortCode
    deriving Show Eq
|]

migrateClientId :: Pool SqlBackend -> IO ()
migrateClientId pool = do
    putStrLn "Starting clientId migration..."
    
    -- 1. First add the column as nullable
    result1 <- try $ runSqlPool 
        (rawSql "ALTER TABLE \"short_url\" ADD COLUMN \"client_id\" VARCHAR" []) 
        pool :: IO (Either SomeException [Single Int])
        
    case result1 of
        Left e -> putStrLn $ "Error adding client_id column (might already exist): " ++ show e
        Right _ -> putStrLn "Added client_id column successfully"
    
    -- 2. Update existing records to set a default value
    result2 <- try $ runSqlPool 
        (rawSql "UPDATE \"short_url\" SET client_id = 'migrated' WHERE client_id IS NULL" []) 
        pool :: IO (Either SomeException [Single Int])
        
    case result2 of
        Left e -> putStrLn $ "Error updating existing records: " ++ show e
        Right _ -> putStrLn "Updated existing records with default client_id"
    
    -- 3. Now add the NOT NULL constraint
    result3 <- try $ runSqlPool 
        (rawSql "ALTER TABLE \"short_url\" ALTER COLUMN \"client_id\" SET NOT NULL" []) 
        pool :: IO (Either SomeException [Single Int])
        
    case result3 of
        Left e -> putStrLn $ "Error adding NOT NULL constraint: " ++ show e
        Right _ -> putStrLn "Added NOT NULL constraint successfully"
    
    putStrLn "ClientId migration completed"

-- API request/response types
data CreateShortUrlRequest = CreateShortUrlRequest
    { longUrl :: Text
    , customAlias :: Maybe Text
    , expiresIn :: Maybe Int  -- Expiration in days
    } deriving (Show, Eq, Generic)

instance FromJSON CreateShortUrlRequest
instance ToJSON CreateShortUrlRequest

data ShortUrlResponse = ShortUrlResponse
    { shortUrl :: Text
    , originalUrl :: Text
    , shortCode :: Text
    , createdAt :: UTCTime
    , expiresAt :: Maybe UTCTime
    , clickCount :: Int
    , qrCodeUrl :: Text
    , clientId :: Text
    } deriving (Show, Eq, Generic)

type ClientId = Text

instance FromJSON ShortUrlResponse
instance ToJSON ShortUrlResponse

newtype ErrorResponse = ErrorResponse
    { errorMessage :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON ErrorResponse
instance ToJSON ErrorResponse

data HealthResponse = HealthResponse
    { status :: Text
    , version :: Text
    , timestamp :: UTCTime
    } deriving (Show, Eq, Generic)

instance FromJSON HealthResponse
instance ToJSON HealthResponse

-- Convert database model to API response
toShortUrlResponse :: Text -> ShortUrl -> ShortUrlResponse
toShortUrlResponse baseUrl shortUrl = ShortUrlResponse
    { shortUrl = baseUrl <> "/" <> shortCode
    , originalUrl = shortUrlOriginalUrl shortUrl
    , shortCode = shortCode
    , createdAt = shortUrlCreatedAt shortUrl
    , expiresAt = shortUrlExpiresAt shortUrl
    , clickCount = shortUrlClickCount shortUrl
    , qrCodeUrl = baseUrl <> "/api/qrcode/" <> shortCode
    , clientId = shortUrlClientId shortUrl
    }
  where
    shortCode = shortUrlShortCode shortUrl
