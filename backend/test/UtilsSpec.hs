module UtilsSpec (spec) where

import Test.Hspec
import Utils (validateUrl, sanitizeUrl)
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Utils" $ do
    describe "validateUrl" $ do
      it "accepts valid URLs" $ do
        validateUrl "http://example.com" `shouldBe` True
        validateUrl "https://example.com/path" `shouldBe` True
        validateUrl "https://subdomain.example.com" `shouldBe` True
        validateUrl "http://example.com/path?query=param" `shouldBe` True
        
      it "rejects invalid URLs" $ do
        validateUrl "not-a-url" `shouldBe` False
        validateUrl "ftp://example.com" `shouldBe` False
        validateUrl "http:example.com" `shouldBe` False
        
      it "rejects URLs that are too short" $ do
        validateUrl "http://a.b" `shouldBe` False
        
      it "requires a domain with a dot" $ do
        validateUrl "http://localhost" `shouldBe` False

    describe "sanitizeUrl" $ do
      it "keeps URLs with http:// or https:// unchanged" $ do
        sanitizeUrl "http://example.com" `shouldBe` "http://example.com"
        sanitizeUrl "https://example.com" `shouldBe` "https://example.com"
        
      it "adds https:// to URLs without a protocol" $ do
        sanitizeUrl "example.com" `shouldBe` "https://example.com"
        sanitizeUrl "example.com/path" `shouldBe` "https://example.com/path"

