{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( validateUrl
    , sanitizeUrl
    , UrlValidationError(..)
    , validateUrlSafe
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import Network.URI (parseURI, uriScheme, uriAuthority, uriRegName)
import AppEnv (AppAction, AppError(..), throwAppError)

-- URL validation error types
data UrlValidationError
    = InvalidSyntax
    | MissingProtocol
    | UnsupportedProtocol
    | MissingDomain
    | InvalidDomainFormat
    | URLTooLong Int Int  -- actual length, max length
    deriving (Show, Eq)

-- Check if a URL is valid (enhanced validation)
validateUrl :: Text -> Either UrlValidationError Text
validateUrl url = do
    -- Check if URL is too long (2048 is a common limit)
    let maxLength = 2048
        urlLength = T.length url
    
    if urlLength > maxLength
        then Left $ URLTooLong urlLength maxLength
        else do
            -- Parse URL using Network.URI
            case parseURI (T.unpack url) of
                Nothing -> Left InvalidSyntax
                Just uri -> do
                    -- Check protocol
                    let scheme = uriScheme uri
                    if null scheme
                        then Left MissingProtocol
                        else if scheme /= "http:" && scheme /= "https:"
                            then Left UnsupportedProtocol
                            else do
                                -- Check domain
                                case uriAuthority uri of
                                    Nothing -> Left MissingDomain
                                    Just auth ->
                                        let domain = uriRegName auth
                                        in if T.any (== '.') (T.pack domain)
                                            then Right url  -- Valid URL
                                            else Left InvalidDomainFormat

-- Ensure the URL has a protocol (add https:// if missing)
sanitizeUrl :: Text -> Text
sanitizeUrl url
    | T.isPrefixOf "http://" url || T.isPrefixOf "https://" url = url
    | otherwise = "https://" <> url

-- Validate URL and return AppError if invalid
validateUrlSafe :: Text -> AppAction Text
validateUrlSafe url =
    case validateUrl (sanitizeUrl url) of
        Left InvalidSyntax -> 
            throwAppError $ ValidationError "The URL has invalid syntax"
        Left MissingProtocol -> 
            throwAppError $ ValidationError "The URL is missing a protocol (http:// or https://)"
        Left UnsupportedProtocol -> 
            throwAppError $ ValidationError "The URL has an unsupported protocol (only http:// and https:// are allowed)"
        Left MissingDomain -> 
            throwAppError $ ValidationError "The URL is missing a domain"
        Left InvalidDomainFormat -> 
            throwAppError $ ValidationError "The domain format is invalid"
        Left (URLTooLong actual max) -> 
            throwAppError $ ValidationError $ 
                "The URL is too long (" <> T.pack (show actual) <> 
                " characters, maximum is " <> T.pack (show max) <> ")"
        Right validUrl -> 
            return validUrl