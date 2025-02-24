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
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Models
import Network.Wai (Request)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

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
        throwError err500 { errBody = "Database error: " <> T.encodeUtf8 msg }
    throwServantError (ValidationError msg) = 
        throwError err400 { errBody = "Validation error: " <> T.encodeUtf8 msg }
    throwServantError RateLimitError = 
        throwError err429 { errBody = "Rate limit exceeded. Please try again later." }
    throwServantError (ResourceNotFound msg) = 
        throwError err404 { errBody = "Not found: " <> T.encodeUtf8 msg }
    throwServantError (SecurityError msg) = 
        throwError err403 { errBody = "Security error: " <> T.encodeUtf8 msg }
    throwServantError (GeneralError msg) = 
        throwError err500 { errBody = "General error: " <> T.encodeUtf8 msg }

-- API server implementation
apiServer :: AppEnv -> Server API
apiServer env = 
      (\req body -> appActionToHandler env req (shortenUrlHandler req body))
 :<|> (appActionToHandler env undefined . getUrlInfoHandler)
 :<|> (\shortCode mSize req -> appActionToHandler env req (generateQRCodeHandler req shortCode mSize))
 :<|> (\shortCode req -> appActionToHandler env req (redirectHandler req shortCode))

-- Create a Servant application from our API
apiHandler :: AppEnv -> Application
apiHandler env = serve (Proxy :: Proxy API) (apiServer env)