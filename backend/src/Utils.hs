{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( validateUrl
    , sanitizeUrl
    ) where

import Data.Text (Text)
import qualified Data.Text as T

-- Check if a URL is valid (basic validation)
validateUrl :: Text -> Bool
validateUrl url =
    -- Check if url starts with http:// or https://
    let hasProtocol = T.isPrefixOf "http://" url || T.isPrefixOf "https://" url
        -- Check minimum length for a valid URL
        minLength = T.length url >= 10
        -- Check if URL contains a domain part
        hasDomain = T.any (== '.') url
    in hasProtocol && minLength && hasDomain

-- Ensure the URL has a protocol (add https:// if missing)
sanitizeUrl :: Text -> Text
sanitizeUrl url
    | T.isPrefixOf "http://" url || T.isPrefixOf "https://" url = url
    | otherwise = "https://" <> url