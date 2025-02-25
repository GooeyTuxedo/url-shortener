{-# LANGUAGE OverloadedStrings #-}

module Shortener
    ( generateShortCode
    , isValidShortCode
    ) where

import Control.Monad (replicateM)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomRIO)

-- Characters to use for short codes (alphanumeric)
codeChars :: String
codeChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Generate a random short code of specified length
generateShortCode :: Int -> IO Text
generateShortCode codeLength = do
    code <- replicateM codeLength (randomChar codeChars)
    return $ T.pack code
  where
    randomChar :: String -> IO Char
    randomChar cs = do
      idx <- randomRIO (0, length cs - 1)
      return $ cs !! idx

-- Validate a custom short code (alphanumeric and min/max length)
isValidShortCode :: Text -> Bool
isValidShortCode code =
    let len = T.length code
        minLen = 3
        maxLen = 20
    in  len >= minLen && len <= maxLen && T.all isAlphaNum code