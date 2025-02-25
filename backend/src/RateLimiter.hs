module RateLimiter 
    ( RateLimiter
    , RateLimitConfig(..)
    , newRateLimiter
    , checkRateLimit
    , cleanupRateLimiter
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)

-- Configuration for rate limiting
data RateLimitConfig = RateLimitConfig
    { maxRequests :: Int           -- Maximum number of requests
    , perTimeWindow :: Int         -- Time window in seconds
    , cleanupInterval :: Int       -- Cleanup interval in seconds
    }

-- Each key in the rate limiter map stores the count and last access time
data RateInfo = RateInfo
    { requestCount :: Int
    , lastRequestTime :: UTCTime
    }

-- Rate limiter state
data RateLimiter = RateLimiter
    { rateLimiterConfig :: RateLimitConfig
    , rateLimiterMap :: MVar (Map Text RateInfo)
    }

-- Create a new rate limiter and start the cleanup thread
newRateLimiter :: RateLimitConfig -> IO RateLimiter
newRateLimiter config = do
    mapVar <- newMVar Map.empty
    let limiter = RateLimiter config mapVar
    void $ forkIO $ cleanupThread limiter
    return limiter

-- Check if a request should be rate limited
checkRateLimit :: RateLimiter -> Text -> IO Bool
checkRateLimit limiter key = do
    now <- getCurrentTime
    modifyMVar (rateLimiterMap limiter) $ \rateMap -> do
        let config = rateLimiterConfig limiter
            windowSeconds = fromIntegral (perTimeWindow config)
            maxReqs = maxRequests config
            
        case Map.lookup key rateMap of
            -- First request for this key
            Nothing -> do
                let newRateInfo = RateInfo 1 now
                return (Map.insert key newRateInfo rateMap, True)
                
            -- Existing rate info
            Just rateInfo -> do
                let timeDiff = diffUTCTime now (lastRequestTime rateInfo)
                    count = requestCount rateInfo
                    
                -- Reset counter if time window has passed
                if timeDiff > windowSeconds
                    then do
                        let newRateInfo = RateInfo 1 now
                        return (Map.insert key newRateInfo rateMap, True)
                    else do
                        -- Increment counter if under limit, otherwise reject
                        if count < maxReqs
                            then do
                                let newRateInfo = RateInfo (count + 1) now
                                return (Map.insert key newRateInfo rateMap, True)
                            else
                                return (rateMap, False)

-- Clean up expired entries periodically
cleanupThread :: RateLimiter -> IO ()
cleanupThread limiter = forever $ do
    threadDelay (cleanupInterval (rateLimiterConfig limiter) * 1000000)
    cleanupRateLimiter limiter

-- Remove expired entries from the rate limiter
cleanupRateLimiter :: RateLimiter -> IO ()
cleanupRateLimiter limiter = do
    now <- getCurrentTime
    modifyMVar_ (rateLimiterMap limiter) $ \rateMap -> do
        let config = rateLimiterConfig limiter
            windowSeconds = fromIntegral (perTimeWindow config)
            cutoffTime = addUTCTime (- windowSeconds) now
            
            -- Keep only entries that aren't expired
            isNotExpired (_, rateInfo) = lastRequestTime rateInfo > cutoffTime
            
        return $ Map.fromList $ filter isNotExpired $ Map.toList rateMap