{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module ApiHandlers
    ( shortenUrlHandler
    , getUrlInfoHandler
    , generateQRCodeHandler
    , redirectHandler
    ) where

import AbuseProtection (isUrlSafe)
import AppEnv
import Config (AppConfig(..))
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist ((==.), Entity(..), get, insert, selectFirst, update, updateGet, (+=.))
import Database.Persist.Sql (SqlBackend, runSqlPool)
import IPUtils (getClientIP, ClientIP(..))
import Models
import Network.HTTP.Types (status301, status404)
import Network.Wai (Request)
import QRGenerator (generateQRCode, QROptions(..), defaultQROptions)
import RateLimiter (checkRateLimit)
import Servant
import Shortener (generateShortCode, isValidShortCode)
import Utils (validateUrlSafe)

-- Handler to create a new short URL
shortenUrlHandler :: MonadIO m => Request -> CreateShortUrlRequest -> AppAction ShortUrlResponse
shortenUrlHandler req CreateShortUrlRequest{..} = do
    -- Get environment
    env <- ask
    let config = envConfig env
        pool = envPool env
        rateLimiter = envRateLimiter env
        contentFilter = envContentFilter env
    
    -- Extract client IP from request (for rate limiting)
    let clientIP = getClientIP req
    
    -- Check rate limit
    allowed <- liftIO $ checkRateLimit rateLimiter (unClientIP clientIP)
    unless allowed $
        throwAppError RateLimitError
    
    -- Validate URL format
    validUrl <- validateUrlSafe longUrl
    
    -- Check if URL is safe
    urlSafe <- liftIO $ isUrlSafe contentFilter validUrl
    unless urlSafe $
        throwAppError $ SecurityError "URL has been blocked for security reasons"
    
    now <- liftIO getCurrentTime
    
    -- Calculate expiration time (default: 30 days if not specified)
    let expiryDays = fromMaybe 30 expiresIn
        expiryTime = addUTCTime (fromIntegral expiryDays * 24 * 60 * 60) now
    
    -- Use custom alias or generate a new short code
    shortCode <- case customAlias of
        Just alias | isValidShortCode alias -> do
            -- Check if alias already exists
            existing <- liftIO $ runSqlPool (selectFirst [ShortUrlShortCode ==. alias] []) pool
            case existing of
                Just _ -> throwAppError $ ValidationError "Custom alias already in use"
                Nothing -> return alias
        Just _ -> throwAppError $ ValidationError "Invalid custom alias format"
        Nothing -> liftIO $ generateShortCode (shortCodeLength config)
    
    -- Create new short URL in database
    shortUrlId <- liftIO $ runSqlPool 
        (insert $ ShortUrl validUrl shortCode now (Just expiryTime) 0)
        pool
    
    -- Retrieve the created entity
    shortUrl <- liftIO $ runSqlPool 
        (get shortUrlId)
        pool
    
    case shortUrl of
        Just url -> return $ toShortUrlResponse (baseUrl config) url
        Nothing -> throwAppError $ DatabaseError "Failed to create short URL"

-- Handler to get information about a short URL
getUrlInfoHandler :: MonadIO m => Text -> AppAction ShortUrlResponse
getUrlInfoHandler shortCode = do
    -- Get environment
    env <- ask
    let config = envConfig env
        pool = envPool env
    
    -- Find the short URL in the database
    result <- liftIO $ runSqlPool 
        (selectFirst [ShortUrlShortCode ==. shortCode] [])
        pool
    
    case result of
        Just (Entity _ shortUrl) -> 
            return $ toShortUrlResponse (baseUrl config) shortUrl
        Nothing -> 
            throwAppError $ ResourceNotFound "Short URL not found"

-- Handler to generate QR code for a short URL
generateQRCodeHandler :: MonadIO m => Request -> Text -> Maybe Int -> AppAction ByteString
generateQRCodeHandler req shortCode mSize = do
    -- Get environment
    env <- ask
    let config = envConfig env
        pool = envPool env
        rateLimiter = envRateLimiter env
    
    -- Extract client IP from request (for rate limiting)
    let clientIP = getClientIP req
    
    -- Apply rate limit for QR code generation
    allowed <- liftIO $ checkRateLimit rateLimiter (unClientIP clientIP)
    unless allowed $
        throwAppError RateLimitError
    
    -- Find the short URL in the database
    result <- liftIO $ runSqlPool 
        (selectFirst [ShortUrlShortCode ==. shortCode] [])
        pool
    
    case result of
        Just (Entity _ shortUrl) -> do
            -- Construct the full short URL
            let fullShortUrl = baseUrl config <> "/" <> shortCode
                
                -- Create QR options with custom size if provided
                qrOptions = case mSize of
                    Just size | size >= 100 && size <= 1000 -> 
                        defaultQROptions { qrSize = size }
                    _ -> defaultQROptions
            
            -- Generate the QR code
            case generateQRCode fullShortUrl qrOptions of
                Right qrCodeData -> return qrCodeData
                Left err -> throwAppError $ GeneralError $ T.pack err
                
        Nothing -> 
            throwAppError $ ResourceNotFound "Short URL not found"

-- Handler to redirect from short URL to original URL
redirectHandler :: MonadIO m => Request -> Text -> AppAction (Headers '[Header "Location" Text] NoContent)
redirectHandler req shortCode = do
    -- Get environment
    env <- ask
    let pool = envPool env
        rateLimiter = envRateLimiter env
    
    -- Extract client IP from request (for rate limiting)
    let clientIP = getClientIP req
    
    -- Apply more lenient rate limit for redirects (could be separate limiter)
    allowed <- liftIO $ checkRateLimit rateLimiter (unClientIP clientIP)
    unless allowed $
        throwAppError RateLimitError
    
    result <- liftIO $ runSqlPool 
        (selectFirst [ShortUrlShortCode ==. shortCode] [])
        pool
    
    case result of
        Just (Entity shortUrlId shortUrl) -> do
            -- Increment click count
            _ <- liftIO $ runSqlPool 
                (update shortUrlId [ShortUrlClickCount +=. 1])
                pool
            
            -- Return a 301 redirect with the original URL
            return $ addHeader (shortUrlOriginalUrl shortUrl) NoContent
            
        Nothing -> 
            throwAppError $ ResourceNotFound "Short URL not found"