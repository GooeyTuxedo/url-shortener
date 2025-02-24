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
import Database.Persist ((==.), Entity(..), get, insert, selectFirst, update, updateGet)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Models
import Network.HTTP.Types (status301, status404)
import Network.Wai (Application, responseLBS)
import Servant
import Shortener (generateShortCode, isValidShortCode)

-- Application state
data AppState = AppState
    { appStatePool :: Pool SqlBackend
    , appStateConfig :: AppConfig
    }

-- API type definition
type API = 
       "api" :> "shorten" :> ReqBody '[JSON] CreateShortUrlRequest :> Post '[JSON] ShortUrlResponse
  :<|> "api" :> "urls" :> Capture "shortCode" Text :> Get '[JSON] ShortUrlResponse
  :<|> Capture "shortCode" Text :> Get '[JSON] NoContent

-- Server implementation
server :: AppState -> Server API
server state = shortenUrlHandler state
          :<|> getUrlInfoHandler state
          :<|> redirectHandler state

-- Handler to create a new short URL
shortenUrlHandler :: AppState -> CreateShortUrlRequest -> Handler ShortUrlResponse
shortenUrlHandler AppState{..} CreateShortUrlRequest{..} = do
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

-- Handler to redirect from short URL to original URL
redirectHandler :: AppState -> Text -> Handler NoContent
redirectHandler AppState{..} shortCode = do
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

-- Create a Servant application from our API
app :: AppState -> Application
app state = serve (Proxy :: Proxy API) (server state)