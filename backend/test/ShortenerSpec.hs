{-# LANGUAGE OverloadedStrings #-}

module ShortenerSpec (spec) where

import Test.Hspec
import Shortener (generateShortCode, isValidShortCode)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (replicateM)
import Data.Char (isAlphaNum)

spec :: Spec
spec = do
  describe "Shortener" $ do
    describe "generateShortCode" $ do
      it "generates a short code of the specified length" $ do
        code <- generateShortCode 6
        T.length code `shouldBe` 6

      it "generates alphanumeric codes" $ do
        code <- generateShortCode 10
        all isAlphaNum (T.unpack code) `shouldBe` True
        
      it "generates different codes on different calls" $ do
        codes <- replicateM 5 (generateShortCode 8)
        length (filter (== head codes) (tail codes)) `shouldBe` 0

    describe "isValidShortCode" $ do
      it "accepts valid short codes" $ do
        isValidShortCode (T.pack "abc123") `shouldBe` True
        isValidShortCode (T.pack "ABC123") `shouldBe` True
        isValidShortCode (T.pack "abcDEF123") `shouldBe` True
        
      it "rejects short codes that are too short" $ do
        isValidShortCode (T.pack "ab") `shouldBe` False
        
      it "rejects short codes that are too long" $ do
        isValidShortCode (T.replicate 25 (T.pack "a")) `shouldBe` False
        
      it "rejects short codes with non-alphanumeric characters" $ do
        isValidShortCode (T.pack "abc-123") `shouldBe` False
        isValidShortCode (T.pack "abc_123") `shouldBe` False
        isValidShortCode (T.pack "abc 123") `shouldBe` False
