{-# LANGUAGE OverloadedStrings #-}

module Middleware
    ( securityHeaders
    , rateLimitMiddleware
    ) where

import Network.Wai
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status429, hContentType)
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
    in res { responseHeaders = secHeaders ++ headers }

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
    -- For simplicity, we'll just use remote host or fallback to a default
    case remoteHost req of
        Nothing -> "unknown"
        Just _ -> "client"  -- In real code, convert the SockAddr to Text