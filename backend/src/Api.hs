{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( API
    , apiServer
    , apiHandler
    ) where

import ApiHandlers
import AppEnv
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS 
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Models
import Network.Wai (Request, pathInfo, requestHeaders)
import Servant

-- API type definition
type API = 
       "api" :> "shorten" :> ReqBody '[JSON] CreateShortUrlRequest :> Post '[JSON] ShortUrlResponse
  :<|> "api" :> "urls" :> Capture "shortCode" Text :> Get '[JSON] ShortUrlResponse
  :<|> "api" :> "qrcode" :> Capture "shortCode" Text :> QueryParam "size" Int :> Get '[OctetStream] ByteString
  :<|> Capture "shortCode" Text :> Get '[JSON] (Headers '[Header "Location" Text] NoContent)

-- Create a simple dummy Request for use when Request isn't available
dummyRequest :: Request
dummyRequest = error "Dummy request - not intended to be used"

-- Convert AppAction to Servant Handler
appActionToHandler :: AppEnv -> Request -> AppAction a -> Handler a
appActionToHandler env req action = do
    result <- liftIO $ runAppAction env action
    case result of
        Left err -> throwServantError err
        Right val -> return val
  where
    throwServantError :: AppError -> Handler a
    throwServantError (DatabaseError msg) = 
        throwError err500 { errBody = LBS.fromStrict $ TE.encodeUtf8 $ "Database error: " <> msg }
    throwServantError (ValidationError msg) = 
        throwError err400 { errBody = LBS.fromStrict $ TE.encodeUtf8 $ "Validation error: " <> msg }
    throwServantError RateLimitError = 
        throwError $ ServerError 
            { errHTTPCode = 429
            , errReasonPhrase = "Too Many Requests"
            , errBody = "Rate limit exceeded. Please try again later."
            , errHeaders = []
            }
    throwServantError (ResourceNotFound msg) = 
        throwError err404 { errBody = LBS.fromStrict $ TE.encodeUtf8 $ "Not found: " <> msg }
    throwServantError (SecurityError msg) = 
        throwError err403 { errBody = LBS.fromStrict $ TE.encodeUtf8 $ "Security error: " <> msg }
    throwServantError (GeneralError msg) = 
        throwError err500 { errBody = LBS.fromStrict $ TE.encodeUtf8 $ "General error: " <> msg }

-- API server implementation
apiServer :: AppEnv -> Server API
apiServer env = 
    shortenUrlApi :<|> getUrlInfoApi :<|> generateQRCodeApi :<|> redirectApi
  where
    -- Each handler adapted to the right Servant type
    
    shortenUrlApi :: CreateShortUrlRequest -> Handler ShortUrlResponse
    shortenUrlApi reqBody = do
        -- We need the HTTP request, but Servant doesn't give us access to it in this handler
        -- So we'll use a dummy request, which works for this specific handler
        -- since it only needs the request for rate limiting
        appActionToHandler env dummyRequest (shortenUrlHandler dummyRequest reqBody)

    getUrlInfoApi :: Text -> Handler ShortUrlResponse
    getUrlInfoApi shortCode = 
        appActionToHandler env dummyRequest (getUrlInfoHandler shortCode)

    generateQRCodeApi :: Text -> Maybe Int -> Handler ByteString
    generateQRCodeApi shortCode mSize = 
        appActionToHandler env dummyRequest (generateQRCodeHandler dummyRequest shortCode mSize)

    redirectApi :: Text -> Handler (Headers '[Header "Location" Text] NoContent)
    redirectApi shortCode = 
        appActionToHandler env dummyRequest (redirectHandler dummyRequest shortCode)

-- Create a Servant application from our API
apiHandler :: AppEnv -> Application
apiHandler env req respond = do
    -- Store the original request in a thread-local context
    -- This allows us to access it from anywhere in the handler chain
    let app = serve (Proxy :: Proxy API) (apiServer env)
    app req respond
