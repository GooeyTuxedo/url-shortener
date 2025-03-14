{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AbuseProtection
    ( UrlContentFilter
    , newUrlContentFilter
    , isUrlSafe
    , validateUrlContent
    , BlacklistConfig(..)
    ) where

import Control.Concurrent.MVar
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.IO (IOMode(..), withFile)
import Control.Exception (catch)

-- Configuration for blacklist
data BlacklistConfig = BlacklistConfig
    { blacklistDomains :: Set Text      -- Domains to block
    , blacklistPatterns :: Set Text     -- URL patterns to block
    , blacklistFile :: Maybe FilePath   -- Optional file path for blacklist
    , maxRedirects :: Int               -- Maximum number of redirects to follow
    , maxUrlLength :: Int               -- Maximum URL length to allow
    }

-- URL content filter state
data UrlContentFilter = UrlContentFilter
    { filterConfig :: BlacklistConfig
    , httpManager :: Manager
    , blacklistDomainsVar :: MVar (Set Text)
    , blacklistPatternsVar :: MVar (Set Text)
    }

-- Create a new URL content filter
newUrlContentFilter :: BlacklistConfig -> IO UrlContentFilter
newUrlContentFilter config = do
    -- Create HTTP manager with redirect following
    manager <- newManager $ tlsManagerSettings
        { managerResponseTimeout = responseTimeoutMicro (10 * 1000000)  -- 10 seconds
        }

    -- Initialize blacklist
    domainsVar <- newMVar (blacklistDomains config)
    patternsVar <- newMVar (blacklistPatterns config)

    -- Load blacklist from file if provided
    case blacklistFile config of
        Just filePath -> do
            loadBlacklistFromFile filePath domainsVar patternsVar
            return ()
        Nothing ->
            return ()

    return $ UrlContentFilter config manager domainsVar patternsVar

-- Load blacklist from file
loadBlacklistFromFile :: FilePath -> MVar (Set Text) -> MVar (Set Text) -> IO ()
loadBlacklistFromFile filePath domainsVar patternsVar = do
    content <- TIO.readFile filePath
    let (domains, patterns) = parseBlacklistFile content

    modifyMVar_ domainsVar $ \currentDomains ->
        return $ Set.union currentDomains domains

    modifyMVar_ patternsVar $ \currentPatterns ->
        return $ Set.union currentPatterns patterns

-- Parse blacklist file content
-- Format: domain:example.com or pattern:example/.*
parseBlacklistFile :: Text -> (Set Text, Set Text)
parseBlacklistFile content =
    let lines = T.lines content
        isDomain = T.isPrefixOf "domain:"
        isPattern = T.isPrefixOf "pattern:"

        extractDomain line = if isDomain line
                             then Just $ T.strip $ T.drop 7 line
                             else Nothing

        extractPattern line = if isPattern line
                              then Just $ T.strip $ T.drop 8 line
                              else Nothing

        domains = Set.fromList $
            [d | Just d <- map extractDomain lines, not (T.null d)]

        patterns = Set.fromList $
            [p | Just p <- map extractPattern lines, not (T.null p)]
    in
        (domains, patterns)

-- Check if URL is safe (not in blacklist)
isUrlSafe :: UrlContentFilter -> Text -> IO Bool
isUrlSafe filter url = do
    -- Check URL length
    let urlLen = T.length url
        maxLen = maxUrlLength (filterConfig filter)

    if urlLen > maxLen
        then return False
        else do
            -- Extract domain from URL
            let domain = extractDomain url

            -- Check domain blacklist
            domains <- readMVar (blacklistDomainsVar filter)
            if domain `Set.member` domains
                then return False
                else do
                    -- Check pattern blacklist
                    patterns <- readMVar (blacklistPatternsVar filter)
                    return $ not $ any (T.isInfixOf url) patterns

-- Extract domain from URL
extractDomain :: Text -> Text
extractDomain url =
    let withoutProtocol = case () of
            _ | T.isPrefixOf "https://" url -> T.drop 8 url
              | T.isPrefixOf "http://" url -> T.drop 7 url
              | otherwise -> url
        domainEnd = T.findIndex (== '/') withoutProtocol
    in case domainEnd of
        Just idx -> T.take idx withoutProtocol
        Nothing -> withoutProtocol

-- Validate URL content by making a HEAD request
validateUrlContent :: UrlContentFilter -> Text -> IO Bool
validateUrlContent filter url = do
    let mgr = httpManager filter
        maxRedir = maxRedirects (filterConfig filter)

    -- First check if URL is in blacklist
    safeUrl <- isUrlSafe filter url
    if not safeUrl
        then return False
        else do
            -- Create request
            req <- parseRequest (T.unpack url)
            let request = req
                    { method = "HEAD"
                    , redirectCount = maxRedir
                    }

            -- Make request and check response
            catchHttpException $ do
                response <- httpNoBody request mgr
                let status = statusCode (responseStatus response)
                return $ status >= 200 && status < 400

-- Catch HTTP exceptions and return False
catchHttpException :: IO Bool -> IO Bool
catchHttpException action = catch action $ \(e :: HttpException) ->
    return False