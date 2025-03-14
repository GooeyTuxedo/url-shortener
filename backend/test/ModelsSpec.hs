{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ModelsSpec (spec) where

import Test.Hspec
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay, diffUTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import Models

-- Helper to compare ShortUrls ignoring microseconds in timestamps
shortUrlEqual :: ShortUrl -> ShortUrl -> Bool
shortUrlEqual a b =
  shortUrlOriginalUrl a == shortUrlOriginalUrl b &&
  shortUrlShortCode a == shortUrlShortCode b &&
  shortUrlClickCount a == shortUrlClickCount b &&
  shortUrlClientId a == shortUrlClientId b &&  -- Check client ID
  (abs (diffUTCTime (shortUrlCreatedAt a) (shortUrlCreatedAt b)) < 1) &&
  case (shortUrlExpiresAt a, shortUrlExpiresAt b) of
    (Just ta, Just tb) -> abs (diffUTCTime ta tb) < 1
    (Nothing, Nothing) -> True
    _ -> False

spec :: Spec
spec = do
  describe "Model Properties" $ do
    describe "ShortUrl" $ do
      it "can be converted to ShortUrlResponse" $ do
        now <- liftIO getCurrentTime
        let expiryTime = addUTCTime (30 * nominalDay) now
            shortUrlModel = ShortUrl 
                        "https://example.com"  -- originalUrl
                        "abc123"               -- shortCode
                        now                    -- createdAt
                        (Just expiryTime)      -- expiresAt
                        0                      -- clickCount
                        "test-client"          -- clientId
                        
            baseUrl = "http://short.url"
            response = toShortUrlResponse baseUrl shortUrlModel
            
        -- Test conversion is correct
        shortUrl response `shouldBe` (baseUrl <> "/" <> shortUrlShortCode shortUrlModel)
        originalUrl response `shouldBe` shortUrlOriginalUrl shortUrlModel
        shortCode response `shouldBe` shortUrlShortCode shortUrlModel
        createdAt response `shouldBe` shortUrlCreatedAt shortUrlModel
        expiresAt response `shouldBe` shortUrlExpiresAt shortUrlModel
        clickCount response `shouldBe` shortUrlClickCount shortUrlModel
        clientId response `shouldBe` shortUrlClientId shortUrlModel
        qrCodeUrl response `shouldBe` (baseUrl <> "/api/qrcode/" <> shortUrlShortCode shortUrlModel)
      
    describe "CreateShortUrlRequest" $ do
      it "can be created with all fields" $ do
        let request = CreateShortUrlRequest
                    { longUrl = "https://example.com/long"
                    , customAlias = Just "custom"
                    , expiresIn = Just 7
                    }
        
        longUrl request `shouldBe` "https://example.com/long"
        customAlias request `shouldBe` Just "custom"
        expiresIn request `shouldBe` Just 7
      
      it "can be created with minimal fields" $ do
        let request = CreateShortUrlRequest
                    { longUrl = "https://example.com/long"
                    , customAlias = Nothing
                    , expiresIn = Nothing
                    }
        
        longUrl request `shouldBe` "https://example.com/long"
        customAlias request `shouldBe` Nothing
        expiresIn request `shouldBe` Nothing
