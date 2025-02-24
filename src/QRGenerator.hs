{-# LANGUAGE OverloadedStrings #-}

module QRGenerator
    ( generateQRCode
    , QROptions(..)
    , defaultQROptions
    ) where

import Codec.Picture (DynamicImage, PixelRGB8(..), encodePng, generateImage)
import qualified Codec.QRCode as QR
import qualified Codec.QRCode.JuicyPixels as QRJP
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Options for QR code generation
data QROptions = QROptions
    { qrSize :: Int           -- Size in pixels (width/height)
    , qrBorder :: Int         -- Border width in modules
    , qrErrorLevel :: QR.ErrorLevel  -- Error correction level
    }

-- Default QR code options
defaultQROptions :: QROptions
defaultQROptions = QROptions
    { qrSize = 300            -- 300x300 pixels
    , qrBorder = 4            -- Standard border
    , qrErrorLevel = QR.M     -- Medium error correction
    }

-- Generate a QR code for the given URL and return PNG image data
generateQRCode :: Text -> QROptions -> Either String ByteString
generateQRCode url options = do
    -- Create QR code with the specified text
    qrCode <- QR.encodeText qrErrorLevel QR.Alphanumeric url
    
    -- Convert to image using JuicyPixels
    let image = QRJP.renderQRCode qrSize qrBorder qrCode
    
    -- Return the image as PNG data
    return $ encodePng image
  where
    QROptions{..} = options