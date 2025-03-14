{-# LANGUAGE OverloadedStrings #-}

module RateLimiterSpec (spec) where

import Test.Hspec
import RateLimiter
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM, replicateM_)
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "RateLimiter" $ do
    it "allows requests under the limit" $ do
      -- Create a rate limiter with 5 requests per second
      rateLimiter <- newRateLimiter $ RateLimitConfig 5 1 10

      -- Make 3 requests (under the limit)
      results <- replicateM 3 $ checkRateLimit rateLimiter (T.pack "test-client")

      -- All requests should be allowed
      and results `shouldBe` True

    it "blocks requests over the limit" $ do
      -- Create a rate limiter with 3 requests per second
      rateLimiter <- newRateLimiter $ RateLimitConfig 3 1 10

      -- Make 5 requests (over the limit)
      results <- replicateM 5 $ checkRateLimit rateLimiter (T.pack "test-client-2")

      -- Only first 3 should be allowed
      take 3 results `shouldBe` [True, True, True]
      drop 3 results `shouldBe` [False, False]

    it "resets after the time window" $ do
      -- Create a rate limiter with 2 requests per 1 second
      rateLimiter <- newRateLimiter $ RateLimitConfig 2 1 10

      -- Use up the limit
      replicateM_ 2 $ checkRateLimit rateLimiter (T.pack "test-client-3")

      -- Next request should be blocked
      checkRateLimit rateLimiter (T.pack "test-client-3") >>= (`shouldBe` False)

      -- Wait for time window to pass
      threadDelay 1100000  -- 1.1 second in microseconds

      -- Should be allowed again
      checkRateLimit rateLimiter (T.pack "test-client-3") >>= (`shouldBe` True)

    it "treats different clients separately" $ do
      -- Create a rate limiter with 2 requests per second
      rateLimiter <- newRateLimiter $ RateLimitConfig 2 1 10

      -- Use up the limit for client A
      replicateM_ 2 $ checkRateLimit rateLimiter (T.pack "client-a")

      -- Client A should be blocked
      checkRateLimit rateLimiter (T.pack "client-a") >>= (`shouldBe` False)

      -- But client B should still be allowed
      checkRateLimit rateLimiter (T.pack "client-b") >>= (`shouldBe` True)

    it "cleans up expired entries" $ do
      -- Create a rate limiter with cleanup after 1 second
      rateLimiter <- newRateLimiter $ RateLimitConfig 2 1 1

      -- Use the limiter
      replicateM_ 2 $ checkRateLimit rateLimiter (T.pack "cleanup-test")

      -- Wait for cleanup
      threadDelay 1100000  -- 1.1 seconds

      -- Manually trigger cleanup (normally done by background thread)
      cleanupRateLimiter rateLimiter

      -- Should be allowed again
      checkRateLimit rateLimiter (T.pack "cleanup-test") >>= (`shouldBe` True)
