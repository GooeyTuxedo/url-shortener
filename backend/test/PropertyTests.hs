module PropertyTests (spec) where

import Test.Hspec
import Test.QuickCheck
import Shortener (generateShortCode, isValidShortCode)
import Utils (validateUrl, sanitizeUrl)
import QRGenerator (generateQRCode, QROptions(..), defaultQROptions)
import qualified Codec.QRCode as QR
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

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
          in T.isPrefixOf "http://" sanitized || T.isPrefixOf "https://" sanitized
      
      it "URLs with valid protocols are unchanged by sanitizeUrl" $ property $
        \proto url -> proto `elem` ["http://", "https://"] && not (T.null url) ==>
          let fullUrl = proto <> url
          in sanitizeUrl fullUrl == fullUrl
    
    describe "QR Code Properties" $ do
      it "QR code generation succeeds for any reasonable URL" $ property $
        \url -> not (T.null url) && T.length url < 100 ==> 
          case generateQRCode url defaultQROptions of
            Right _ -> True
            Left _ -> False
      
      it "increasing QR size increases output size" $ property $
        \url size1 size2 -> 
          not (T.null url) && 
          size1 >= 100 && size1 <= 500 && 
          size2 >= 500 && size2 <= 1000 && 
          size1 < size2 ==>
            let options1 = defaultQROptions { qrSize = size1 }
                options2 = defaultQROptions { qrSize = size2 }
            in case (generateQRCode url options1, generateQRCode url options2) of
                 (Right bs1, Right bs2) -> bs1 `smallerThan` bs2
                 _ -> False
                 
-- Custom generator for URLs
genSafeChar :: Gen Char
genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-._~:/?#[]@!$&'()*+,;="

genURL :: Gen Text
genURL = do
  proto <- elements ["http://", "https://"]
  domain <- T.pack <$> listOf1 genSafeChar
  tld <- elements [".com", ".org", ".net", ".io"]
  path <- T.pack <$> listOf genSafeChar
  return $ proto <> domain <> tld <> "/" <> path

-- Helper for comparing ByteString sizes
smallerThan :: ByteString -> ByteString -> Bool
smallerThan a b = LBS.length a < LBS.length b

-- Required instances for custom generators
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  shrink t = T.pack <$> shrink (T.unpack t)

