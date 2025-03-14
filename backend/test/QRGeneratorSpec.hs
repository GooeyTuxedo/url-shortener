{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use let" #-}

module QRGeneratorSpec (spec) where

import Test.Hspec
import QRGenerator (generateQRCode, QROptions(..), defaultQROptions)
import qualified Codec.QRCode as QR
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

spec :: Spec
spec = do
  describe "QRGenerator" $ do
    describe "generateQRCode" $ do
      it "successfully generates a QR code for a valid URL" $ do
        let url = T.pack "https://example.com"
        result <- pure $ generateQRCode url defaultQROptions
        case result of
          Right bs -> LBS.length bs `shouldSatisfy` (> 0)
          Left err -> expectationFailure $ "Failed to generate QR code: " ++ err
          
      it "adjusts the size based on options" $ do
        let url = T.pack "https://example.com"
            smallOptions = defaultQROptions { qrSize = 100 }
            largeOptions = defaultQROptions { qrSize = 500 }
        
        resultSmall <- pure $ generateQRCode url smallOptions
        resultLarge <- pure $ generateQRCode url largeOptions
        
        case (resultSmall, resultLarge) of
          (Right bsSmall, Right bsLarge) -> do
            LBS.length bsSmall `shouldSatisfy` (> 0)
            LBS.length bsLarge `shouldSatisfy` (> 0)
            LBS.length bsLarge `shouldSatisfy` (> LBS.length bsSmall)
          _ -> expectationFailure "Failed to generate QR codes with different sizes"
          
      it "uses the specified error correction level" $ do
        let url = T.pack "https://example.com"
            lowECOptions = defaultQROptions { qrErrorLevel = QR.L }
            highECOptions = defaultQROptions { qrErrorLevel = QR.H }
        
        resultLow <- pure $ generateQRCode url lowECOptions
        resultHigh <- pure $ generateQRCode url highECOptions
        
        case (resultLow, resultHigh) of
          (Right bsLow, Right bsHigh) -> do
            LBS.length bsLow `shouldSatisfy` (> 0)
            LBS.length bsHigh `shouldSatisfy` (> 0)
          _ -> expectationFailure "Failed to generate QR codes with different error levels"
