{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec (spec) where

import Test.Hspec
import Utils (validateUrl, sanitizeUrl, UrlValidationError(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (isRight, isLeft)

spec :: Spec
spec = do
  describe "Utils" $ do
    describe "validateUrl" $ do
      it "accepts valid URLs" $ do
        validateUrl (T.pack "http://example.com") `shouldSatisfy` isRight
        validateUrl (T.pack "https://example.com/path") `shouldSatisfy` isRight
        validateUrl (T.pack "https://subdomain.example.com") `shouldSatisfy` isRight
        validateUrl (T.pack "http://example.com/path?query=param") `shouldSatisfy` isRight
        
      it "rejects invalid URLs" $ do
        validateUrl (T.pack "not-a-url") `shouldSatisfy` isLeft
        validateUrl (T.pack "ftp://example.com") `shouldSatisfy` isLeft
        validateUrl (T.pack "http:example.com") `shouldSatisfy` isLeft
        
      it "rejects URLs that are too long" $ do
        validateUrl (T.replicate 3000 (T.pack "a")) `shouldSatisfy` isLeft
        
      it "requires http or https protocol" $ do
        validateUrl (T.pack "example.com") `shouldSatisfy` isLeft
        case validateUrl (T.pack "example.com") of
          Left MissingProtocol -> return ()
          _ -> expectationFailure "Should fail with MissingProtocol"

    describe "sanitizeUrl" $ do
      it "keeps URLs with http:// or https:// unchanged" $ do
        sanitizeUrl (T.pack "http://example.com") `shouldBe` (T.pack "http://example.com")
        sanitizeUrl (T.pack "https://example.com") `shouldBe` (T.pack "https://example.com")
        
      it "adds https:// to URLs without a protocol" $ do
        sanitizeUrl (T.pack "example.com") `shouldBe` (T.pack "https://example.com")
        sanitizeUrl (T.pack "example.com/path") `shouldBe` (T.pack "https://example.com/path")
