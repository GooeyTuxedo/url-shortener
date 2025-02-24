{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( API
    , app
    ) where

import Config (AppConfig(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Database.Persist ((==.), Entity(..), get, insert, selectFirst, update, updateGet, (+=.))
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Models
import Network.HTTP.Types (status301, status404)
import Network.Wai (Application, responseLBS)
import Servant
import Shortener (generateShortCode, isValidShortCode)
import QRGenerator (generateQRCode, QROptions(..), defaultQROptions)
import Data.ByteString.Lazy (ByteString)
import AbuseProtection (isUrlSafe)
import RateLimiter (checkRateLimit)
import Control.Monad (unless)

-- Application state
data AppState = AppState
    { appStatePool :: Pool SqlBackend
    , appStateConfig :: AppConfig
    , appStateRateLimiter :: RateLimiter
    , appStateContentFilter :: UrlContentFilter
    , appStateHttpManager :: Manager
    }

-- API type definition
type API = 
       "api" :> "shorten" :> ReqBody '[JSON] CreateShortUrlRequest :> Post '[JSON] ShortUrlResponse
  :<|> "api" :> "urls" :> Capture "shortCode" Text :> Get '[JSON] ShortUrlResponse
  :<|> "api" :> "qrcode" :> Capture "shortCode" Text :> QueryParam "size" Int :> Get '[OctetStream] ByteString
  :<|> Capture "shortCode" Text :> Get '[JSON] NoContent

-- Server implementation
server :: AppState -> Server API
server state = shortenUrlHandler state
          :<|> getUrlInfoHandler state
          :<|> generateQRCodeHandler state
          :<|> redirectHandler state

-- Handler to create a new short URL
shortenUrlHandler :: AppState -> CreateShortUrlRequest -> Handler ShortUrlResponse
shortenUrlHandler state@AppState{..} CreateShortUrlRequest{..} = do
    -- Extract client IP from request (for rate limiting)
    clientIP <- liftIO $ getClientIP
    
    -- Check rate limit
    allowed <- liftIO $ checkRateLimit appStateRateLimiter clientIP
    unless allowed $
        throwError err429 { errBody = "Rate limit exceeded. Please try again later." }
    
    -- Check if URL is safe
    urlSafe <- liftIO $ isUrlSafe appStateContentFilter longUrl
    unless urlSafe $
        throwError err400 { errBody = "URL has been blocked for security reasons" }
    
    now <- liftIO getCurrentTime
    
    -- Calculate expiration time (default: 30 days if not specified)
    let expiryDays = fromMaybe 30 expiresIn
        expiryTime = addUTCTime (fromIntegral expiryDays * 24 * 60 * 60) now
    
    -- Use custom alias or generate a new short code
    shortCode <- case customAlias of
        Just alias | isValidShortCode alias -> do
            -- Check if alias already exists
            existing <- liftIO $ runSqlPool (selectFirst [ShortUrlShortCode ==. alias] []) appStatePool
            case existing of
                Just _ -> throwError err409 { errBody = "Custom alias already in use" }
                Nothing -> return alias
        Just _ -> throwError err400 { errBody = "Invalid custom alias format" }
        Nothing -> liftIO $ generateShortCode (shortCodeLength appStateConfig)
    
    -- Create new short URL in database
    shortUrlId <- liftIO $ runSqlPool 
        (insert $ ShortUrl longUrl shortCode now (Just expiryTime) 0)
        appStatePool
    
    -- Retrieve the created entity
    shortUrl <- liftIO $ runSqlPool 
        (get shortUrlId)
        appStatePool
    
    case shortUrl of
        Just url -> return $ toShortUrlResponse (baseUrl appStateConfig) url
        Nothing -> throwError err500 { errBody = "Failed to create short URL" }
  where
    -- Helper function to get client IP from request
    getClientIP :: IO Text
    getClientIP = return "0.0.0.0"  -- In production, extract from request headers

-- Handler to get information about a short URL
getUrlInfoHandler :: AppState -> Text -> Handler ShortUrlResponse
getUrlInfoHandler AppState{..} shortCode = do
    result <- liftIO $ runSqlPool 
        (selectFirst [ShortUrlShortCode ==. shortCode] [])
        appStatePool
    
    case result of
        Just (Entity _ shortUrl) -> 
            return $ toShortUrlResponse (baseUrl appStateConfig) shortUrl
        Nothing -> 
            throwError err404 { errBody = "Short URL not found" }

-- Handler to generate QR code for a short URL
generateQRCodeHandler :: AppState -> Text -> Maybe Int -> Handler ByteString
generateQRCodeHandler AppState{..} shortCode mSize = do
    -- Extract client IP from request (for rate limiting)
    clientIP <- liftIO $ getClientIP
    
    -- Apply rate limit for QR code generation
    allowed <- liftIO $ checkRateLimit appStateRateLimiter clientIP
    unless allowed $
        throwError err429 { errBody = "Rate limit exceeded. Please try again later." }
    
    -- Find the short URL in the database
    result <- liftIO $ runSqlPool 
        (selectFirst [ShortUrlShortCode ==. shortCode] [])
        appStatePool
    
    case result of
        Just (Entity _ shortUrl) -> do
            -- Construct the full short URL
            let fullShortUrl = baseUrl appStateConfig <> "/" <> shortCode
                
                -- Create QR options with custom size if provided
                qrOptions = case mSize of
                    Just size | size >= 100 && size <= 1000 -> 
                        defaultQROptions { qrSize = size }
                    _ -> defaultQROptions
            
            -- Generate the QR code
            case generateQRCode fullShortUrl qrOptions of
                Right qrCodeData -> return qrCodeData
                Left err -> throwError err500 { errBody = "Failed to generate QR code" }
                
        Nothing -> 
            throwError err404 { errBody = "Short URL not found" }
  where
    -- Helper function to get client IP from request
    getClientIP :: IO Text
    getClientIP = return "0.0.0.0"  -- In production, extract from request headers

-- Handler to redirect from short URL to original URL
redirectHandler :: AppState -> Text -> Handler NoContent
redirectHandler AppState{..} shortCode = do
    -- Extract client IP from request (for rate limiting)
    clientIP <- liftIO $ getClientIP
    
    -- Apply more lenient rate limit for redirects (could be separate limiter)
    allowed <- liftIO $ checkRateLimit appStateRateLimiter clientIP
    unless allowed $
        throwError err429 { errBody = "Rate limit exceeded. Please try again later." }
    
    result <- liftIO $ runSqlPool 
        (selectFirst [ShortUrlShortCode ==. shortCode] [])
        appStatePool
    
    case result of
        Just (Entity shortUrlId shortUrl) -> do
            -- Increment click count
            _ <- liftIO $ runSqlPool 
                (update shortUrlId [ShortUrlClickCount +=. 1])
                appStatePool
            
            -- Throw a 301 redirect with the original URL
            throwError err301 
                { errHeaders = [("Location", T.encodeUtf8 $ shortUrlOriginalUrl shortUrl)]
                , errBody = ""
                }
        Nothing -> 
            throwError err404 { errBody = "Short URL not found" }
  where
    -- Helper function to get client IP from request
    getClientIP :: IO Text
    getClientIP = return "0.0.0.0"  -- In production, extract from request headers

-- Create a Servant application from our API
app :: AppState -> Application
app state = serve (Proxy :: Proxy API) (server state)