{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module QRGenerator
    ( generateQRCode
    , QROptions(..)
    , defaultQROptions
    ) where

import Codec.Picture (DynamicImage, PixelRGB8(..), encodePng)
import qualified Codec.QRCode as QR
import qualified Codec.QRCode.Data.TextEncoding as TE
import qualified Codec.QRCode.JuicyPixels as QRJP
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

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
generateQRCode url QROptions{..} = 
    -- Create QR code with the specified text
    case QR.encodeText (QR.defaultQRCodeOptions qrErrorLevel) TE.Utf8WithECI url of
        Nothing -> Left "Failed to generate QR code"
        Just qrCode -> 
            -- Convert to image using JuicyPixels
            Right $ encodePng (QRJP.toImage qrSize qrBorder qrCode)