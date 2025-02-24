{-# LANGUAGE OverloadedStrings #-}

module IPUtils
    ( getClientIP
    , ClientIP(..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Socket (SockAddr(..), HostAddress, hostAddressToTuple)
import Network.Wai (Request, remoteHost, requestHeaders)

-- Newtype for client IP to improve type safety
newtype ClientIP = ClientIP { unClientIP :: Text }
    deriving (Show, Eq, Ord)

-- Extract client IP from request
getClientIP :: Request -> ClientIP
getClientIP req = ClientIP $ fromMaybe (sockAddrToText $ remoteHost req) forwardedIP
  where
    -- Try to get IP from X-Forwarded-For header
    forwardedIP = do
        forwardedHeader <- lookup "X-Forwarded-For" (requestHeaders req)
        let ips = B8.split ',' forwardedHeader
        if null ips
            then Nothing
            else Just $ TE.decodeUtf8 $ B8.strip $ head ips

-- Convert SockAddr to Text representation
sockAddrToText :: SockAddr -> Text
sockAddrToText (SockAddrInet _ hostAddr) =
    let (a, b, c, d) = hostAddressToTuple hostAddr
    in T.pack $ show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
sockAddrToText (SockAddrInet6 _ _ hostAddr6 _) =
    T.pack "ipv6" -- Simplified for this example
sockAddrToText _ = 
    T.pack "unknown"
