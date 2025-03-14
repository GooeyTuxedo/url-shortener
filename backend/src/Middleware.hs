{-# LANGUAGE OverloadedStrings #-}

module Middleware
    ( optionsMiddleware
    , securityHeaders
    , rateLimitMiddleware
    ) where

import Network.Wai
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status200, status429, hContentType, methodOptions, ResponseHeaders)
import Network.HTTP.Types.Header (hContentType, hAuthorization, Header)
import RateLimiter (RateLimiter, checkRateLimit)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

-- Add security headers to responses
securityHeaders :: Middleware
securityHeaders app req respond = app req $ \res -> respond $
    let headers = responseHeaders res
        secHeaders = 
            [ ("X-Content-Type-Options", "nosniff")
            , ("X-Frame-Options", "DENY")
            , ("X-XSS-Protection", "1; mode=block")
            , ("Content-Security-Policy", "default-src 'self'")
            , ("Referrer-Policy", "strict-origin-when-cross-origin")
            ]
    in mapResponseHeaders (secHeaders ++) res

-- Rate limiting middleware using our RateLimiter module
rateLimitMiddleware :: RateLimiter -> Middleware
rateLimitMiddleware rateLimiter app req respond = do
    -- Get client IP or some other identifier from request
    let clientId = getClientId req
    
    -- Check rate limit
    allowed <- liftIO $ checkRateLimit rateLimiter clientId
    
    if allowed
        then app req respond
        else respond $ responseLBS 
            status429 
            [(hContentType, "application/json")] 
            "{\"error\":\"Rate limit exceeded. Please try again later.\"}"

-- Extract client identifier (IP address) from request
getClientId :: Request -> Text
getClientId req =
    -- In production, you'd extract IP from X-Forwarded-For or other headers
    -- For simplicity, we'll just use a default
    "client-ip"

-- Middleware to handle OPTIONS preflight requests
optionsMiddleware :: Middleware
optionsMiddleware app req respond =
    if requestMethod req == methodOptions
    then
        -- Respond directly to OPTIONS requests with proper CORS headers
        respond $ responseLBS
            status200
            [ ("Access-Control-Allow-Origin", "*")
            , ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
            , ("Access-Control-Allow-Headers", "Content-Type, Authorization, Accept, X-Requested-With")
            , ("Access-Control-Max-Age", "3600")
            , (hContentType, "text/plain")
            ]
            LBS.empty
    else
        -- For non-OPTIONS requests, pass through to the next middleware
        -- and add CORS headers to the response
        app req $ \res ->
            respond $ mapResponseHeaders addCorsHeaders res

-- Add CORS headers to every response
addCorsHeaders :: [Header] -> [Header]
addCorsHeaders headers =
    corsHeaders ++ headers
  where
    corsHeaders =
        [ ("Access-Control-Allow-Origin", "*")
        , ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
        , ("Access-Control-Allow-Headers", "Content-Type, Authorization, Accept, X-Requested-With")
        ]
