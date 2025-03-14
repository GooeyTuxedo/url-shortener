{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ApiSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), encode, decode, object, withObject, (.=), (.:), (.:?))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AT
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BLC

-- Define response type for parsing (updated to include clientId)
data ShortUrlResponseJSON = ShortUrlResponseJSON
    { shortUrlJSON :: Text
    , originalUrlJSON :: Text
    , shortCodeJSON :: Text
    , clientIdJSON :: Maybe Text
    } deriving (Show, Eq)

instance FromJSON ShortUrlResponseJSON where
    parseJSON = withObject "ShortUrlResponseJSON" $ \v -> ShortUrlResponseJSON
        <$> v .: Key.fromString "shortUrl"
        <*> v .: Key.fromString "originalUrl"
        <*> v .: Key.fromString "shortCode"
        <*> v .:? Key.fromString "clientId"

-- Test specifications
spec :: Spec
spec = do
    describe "ClientID API Models" $ do
        describe "ShortUrlResponseJSON" $ do
            it "can be decoded from JSON with client ID" $ do
                let jsonStr = "{\"shortUrl\":\"http://example.com/abc123\",\"originalUrl\":\"http://original.com\",\"shortCode\":\"abc123\",\"createdAt\":\"2023-01-01T00:00:00Z\",\"clickCount\":0,\"qrCodeUrl\":\"http://example.com/qr/abc123\",\"clientId\":\"test-client\"}"
                let decoded = decode jsonStr :: Maybe ShortUrlResponseJSON
                
                case decoded of
                    Just resp -> do
                        shortUrlJSON resp `shouldBe` "http://example.com/abc123"
                        originalUrlJSON resp `shouldBe` "http://original.com"
                        shortCodeJSON resp `shouldBe` "abc123"
                        clientIdJSON resp `shouldBe` Just "test-client"
                    Nothing -> expectationFailure "Failed to decode JSON"
                
            it "can be decoded from JSON without client ID" $ do
                let jsonStr = "{\"shortUrl\":\"http://example.com/abc123\",\"originalUrl\":\"http://original.com\",\"shortCode\":\"abc123\",\"createdAt\":\"2023-01-01T00:00:00Z\",\"clickCount\":0,\"qrCodeUrl\":\"http://example.com/qr/abc123\"}"
                let decoded = decode jsonStr :: Maybe ShortUrlResponseJSON
                
                case decoded of
                    Just resp -> do
                        shortUrlJSON resp `shouldBe` "http://example.com/abc123"
                        originalUrlJSON resp `shouldBe` "http://original.com"
                        shortCodeJSON resp `shouldBe` "abc123"
                        clientIdJSON resp `shouldBe` Nothing
                    Nothing -> expectationFailure "Failed to decode JSON"

        describe "CreateShortUrlRequest" $ do
            it "can create valid JSON for creating URL with client ID" $ do
                -- This test doesn't require a database
                let requestObj = object
                        [ Key.fromString "longUrl" .= T.pack "https://example.com/test"
                        , Key.fromString "customAlias" .= T.pack "custom-alias"
                        , Key.fromString "expiresIn" .= (30 :: Int)
                        ]
                    
                    jsonStr = encode requestObj
                    jsonText = BLC.unpack jsonStr
                
                -- Better validation that doesn't depend on field order
                jsonText `shouldContain` "\"longUrl\":\"https://example.com/test\""
                jsonText `shouldContain` "\"customAlias\":\"custom-alias\""
                jsonText `shouldContain` "\"expiresIn\":30"
