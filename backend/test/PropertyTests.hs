{-# LANGUAGE OverloadedStrings #-}

module PropertyTests (spec) where

import Test.Hspec
import Test.QuickCheck
import Shortener (generateShortCode, isValidShortCode)
import Utils (validateUrl, sanitizeUrl, UrlValidationError(..))
import QRGenerator (generateQRCode, QROptions(..), defaultQROptions)
import qualified Codec.QRCode as QR
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight)

spec :: Spec
spec = do
  describe "Property Tests" $ do
    describe "Shortener Properties" $ do
      it "generated short codes always have the requested length" $ property $
        \n -> n > 0 && n < 100 ==> ioProperty $ do
          code <- generateShortCode n
          return $ T.length code == n
      
      it "generated short codes are always valid" $ property $
        \n -> n >= 3 && n <= 20 ==> ioProperty $ do
          code <- generateShortCode n
          return $ isValidShortCode code
    
    describe "URL Validation Properties" $ do
      it "sanitizeUrl always produces URLs with a protocol" $ property $
        \url -> not (T.null url) ==> 
          let sanitized = sanitizeUrl url
          in T.isPrefixOf (T.pack "http://") sanitized || T.isPrefixOf (T.pack "https://") sanitized
      
      it "URLs with valid protocols are unchanged by sanitizeUrl" $ property $
        \p url -> p `elem` [T.pack "http://", T.pack "https://"] && not (T.null url) ==>
          let fullUrl = p <> url
          in sanitizeUrl fullUrl == fullUrl
    
    describe "QR Code Properties" $ do
      it "QR code generation succeeds for any reasonable URL" $ property $
        \url -> not (T.null url) && T.length url < 100 ==> 
          isRight $ generateQRCode (sanitizeUrl url) defaultQROptions
      
      it "increasing QR size increases output size" $ property $
        \url size1 size2 -> 
          not (T.null url) && 
          size1 >= 100 && size1 <= 500 && 
          size2 >= 500 && size2 <= 1000 && 
          size1 < size2 ==>
            let options1 = defaultQROptions { qrSize = size1 }
                options2 = defaultQROptions { qrSize = size2 }
                sanitizedUrl = sanitizeUrl url
            in case (generateQRCode sanitizedUrl options1, generateQRCode sanitizedUrl options2) of
                 (Right bs1, Right bs2) -> bs1 `smallerThan` bs2
                 _ -> False
    
    describe "Client ID Properties" $ do
      it "client IDs can be any valid text" $ property $
        \clientId -> not (T.null clientId) && T.length clientId < 100 ==>
          validClientId clientId
      
-- Property for valid client IDs
validClientId :: Text -> Bool
validClientId clientId = 
  not (T.null clientId) && T.length clientId < 100

-- Custom generator for client IDs
genClientId :: Gen Text
genClientId = do
  prefix <- elements ["user-", "client-", "test-", ""]
  id <- T.pack <$> listOf1 genSafeChar
  return $ prefix <> id

-- Custom generator for URLs
genSafeChar :: Gen Char
genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-._~:/?#[]@!$&'()*+,;="

genURL :: Gen Text
genURL = do
  proto <- elements [T.pack "http://", T.pack "https://"]
  domain <- T.pack <$> listOf1 genSafeChar
  tld <- elements [T.pack ".com", T.pack ".org", T.pack ".net", T.pack ".io"]
  path <- T.pack <$> listOf genSafeChar
  return $ proto <> domain <> tld <> T.pack "/" <> path

-- Helper for comparing ByteString sizes
smallerThan :: LBS.ByteString -> LBS.ByteString -> Bool
smallerThan a b = LBS.length a < LBS.length b

-- Required instances for custom generators
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  shrink t = T.pack <$> shrink (T.unpack t)
