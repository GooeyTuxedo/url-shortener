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
import AppEnv (AppEnv(..), AppError(..), AppAction, runAppAction, throwAppError)
import Control.Monad.Reader (ask)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS 
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Models
import Network.Wai (Request, pathInfo, requestHeaders, Application, Response, ResponseReceived, responseLBS, defaultRequest)
import Network.HTTP.Types (Status, status200, status301, status400, status403, status404, status500, mkStatus)
import Data.Aeson (ToJSON, encode)
import Servant
import Database.Persist ((==.), Entity(..), selectFirst, update, (+=.))
import Database.Persist.Sql (runSqlPool)

-- API type definition
type API = 
       "api" :> "shorten" :> ReqBody '[JSON] CreateShortUrlRequest :> Post '[JSON] ShortUrlResponse
  :<|> "api" :> "urls" :> Capture "shortCode" Text :> Get '[JSON] ShortUrlResponse
  :<|> "api" :> "qrcode" :> Capture "shortCode" Text :> QueryParam "size" Int :> Get '[OctetStream] ByteString
  :<|> Capture "shortCode" Text :> Get '[JSON] (Headers '[Header "Location" Text] NoContent)

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

-- A proper request that can be used in tests
testRequest :: Request
testRequest = defaultRequest

-- API server implementation
apiServer :: AppEnv -> Server API
apiServer env = 
    shortenUrlApi :<|> getUrlInfoApi :<|> generateQRCodeApi :<|> redirectApi
  where
    -- Each handler adapted to the right Servant type
    
    shortenUrlApi :: CreateShortUrlRequest -> Handler ShortUrlResponse
    shortenUrlApi reqBody = do
        appActionToHandler env testRequest (shortenUrlHandler testRequest reqBody)

    getUrlInfoApi :: Text -> Handler ShortUrlResponse
    getUrlInfoApi shortCode = 
        appActionToHandler env testRequest (getUrlInfoHandler shortCode)

    generateQRCodeApi :: Text -> Maybe Int -> Handler ByteString
    generateQRCodeApi shortCode mSize = 
        appActionToHandler env testRequest (generateQRCodeHandler testRequest shortCode mSize)

    redirectApi :: Text -> Handler (Headers '[Header "Location" Text] NoContent)
    redirectApi shortCode = 
        appActionToHandler env testRequest (redirectHandler testRequest shortCode)

-- Create a Servant application from our API
apiHandler :: AppEnv -> Application
apiHandler env = serve (Proxy :: Proxy API) (apiServer env)
