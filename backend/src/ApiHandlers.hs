{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ApiHandlers
    ( shortenUrlHandler
    , getUrlInfoHandler
    , generateQRCodeHandler
    , redirectHandler
    , healthHandler
    , getUrlsForClientHandler
    ) where

import AbuseProtection (isUrlSafe)
import AppEnv
import Config (AppConfig(..))
import Control.Exception (try, SomeException)
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
import Database.Persist ((==.), Entity(..), get, insert, selectFirst, selectList, update, updateGet, (+=.))
import Database.Persist.Sql (SqlBackend, runSqlPool, rawSql, Single(..))
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
shortenUrlHandler :: Request -> ClientId -> CreateShortUrlRequest -> AppAction ShortUrlResponse
shortenUrlHandler req clientId CreateShortUrlRequest{..} = do
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
    
    -- Create new short URL in database with client ID
    shortUrlId <- liftIO $ runSqlPool 
        (insert $ ShortUrl validUrl shortCode now (Just expiryTime) 0 clientId)
        pool
    
    -- Retrieve the created entity
    shortUrl <- liftIO $ runSqlPool 
        (get shortUrlId)
        pool
    
    case shortUrl of
        Just url -> return $ toShortUrlResponse (baseUrl config) url
        Nothing -> throwAppError $ DatabaseError "Failed to create short URL"

-- Handler to get information about a short URL
getUrlInfoHandler :: Text -> Maybe ClientId -> AppAction ShortUrlResponse
getUrlInfoHandler shortCode mClientId = do
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
            -- If clientId is provided, check URL ownership
            case mClientId of
                Just clientId -> 
                    if shortUrlClientId shortUrl == clientId || clientId == "admin"
                        then return $ toShortUrlResponse (baseUrl config) shortUrl
                        else throwAppError $ SecurityError "You don't have permission to view this URL"
                Nothing -> 
                    -- Public access without client ID is allowed
                    return $ toShortUrlResponse (baseUrl config) shortUrl
        Nothing -> 
            throwAppError $ ResourceNotFound "Short URL not found"

getUrlsForClientHandler :: ClientId -> AppAction [ShortUrlResponse]
getUrlsForClientHandler clientId = do
    -- Get environment
    env <- ask
    let config = envConfig env
        pool = envPool env
    
    -- Find all short URLs for this client
    results <- liftIO $ runSqlPool 
        (selectList [ShortUrlClientId ==. clientId] [])
        pool
    
    -- Convert to response format
    return $ map (\(Entity _ url) -> toShortUrlResponse (baseUrl config) url) results

-- Handler to generate QR code for a short URL
generateQRCodeHandler :: Request -> Text -> Maybe Int -> AppAction ByteString
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
redirectHandler :: Request -> Text -> AppAction (Headers '[Header "Location" Text] NoContent)
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

-- Handler to check application health
healthHandler :: AppAction HealthResponse
healthHandler = do
    -- Get environment
    env <- ask
    let config = envConfig env
        pool = envPool env
    
    -- Check database connection with a simple query
    dbStatus <- liftIO $ checkDatabaseConnection pool
    
    -- Get current time
    now <- liftIO getCurrentTime
    
    -- Return health status
    if dbStatus
        then return $ HealthResponse "healthy" "1.0.0" now
        else throwAppError $ GeneralError "Database connection failed"

-- Helper function to check database connection
checkDatabaseConnection :: Pool SqlBackend -> IO Bool
checkDatabaseConnection pool = do
    -- Try a simple query to check the database connection
    result <- try $ runSqlPool (rawSql "SELECT 1" []) pool :: IO (Either SomeException [Single Int])
    case result of
        Left _ -> return False
        Right [Single 1] -> return True
        Right _ -> return False
