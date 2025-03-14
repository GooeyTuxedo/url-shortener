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
