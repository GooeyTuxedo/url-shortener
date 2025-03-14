{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ApiSpec (spec) where

import Test.Hspec
import Network.Wai.Test (Session, SResponse(..), request, runSession, defaultRequest, simpleHeaders)
import Network.Wai.Internal (requestMethod, requestHeaders, requestBody, rawPathInfo, rawQueryString)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Header (HeaderName, hContentType)
import qualified Data.CaseInsensitive as CI
import Network.Wai (Application)
import Network.HTTP.Types (methodGet, methodPost, status200, status301, status404, Header)
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), encode, decode, object, withObject, (.=), (.:), (.:?))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AT
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Config (dbConfig, loadConfig)
import App (initConnectionPool, runMigrations)
import Api (apiHandler)
import AppEnv
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import AbuseProtection (BlacklistConfig(..), newUrlContentFilter)
import RateLimiter (RateLimitConfig(..), newRateLimiter)
import qualified Data.Set as Set
import Models (ShortUrl, ShortUrlId)
import Database.Persist.Sql (Filter, runSqlPool, deleteWhere, (==.))
import Control.Exception (try, SomeException)

-- Setup application for testing
setupApp :: IO Application
setupApp = do
    -- Load configuration
    config <- loadConfig

    -- Initialize database pool
    pool <- initConnectionPool (dbConfig config)

    -- Run migrations
    runMigrations pool

    -- Clean the database before tests
    runSqlPool (deleteWhere ([] :: [Filter ShortUrl])) pool

    -- Create test environment
    rateLimiter <- newRateLimiter $ RateLimitConfig 100 60 300
    contentFilter <- newUrlContentFilter $ BlacklistConfig
        (Set.fromList [])
        (Set.fromList [])
        Nothing
        5
        2048
    httpManager <- newManager tlsManagerSettings

    let env = AppEnv
            { envConfig = config
            , envPool = pool
            , envRateLimiter = rateLimiter
            , envContentFilter = contentFilter
            , envHttpManager = httpManager
            }

    -- Return API handler
    return $ apiHandler env

-- Helper function to create and run a request
doRequest :: Method -> BS.ByteString -> [Header] -> ByteString -> Application -> IO SResponse
doRequest method path headers body app = do
    -- Create a request using Network.Wai.Test methods
    let strictBody = BS.concat $ LBS.toChunks body -- Convert from lazy to strict ByteString
    let req = defaultRequest
                { requestMethod = method
                , requestHeaders = headers
                , requestBody = return strictBody
                , rawPathInfo = path  -- Set the path
                }
    -- Run the request through the session
    runSession (request req) app

-- Helper function to create and run a request with query parameters
doRequestWithQuery :: Method -> BS.ByteString -> BS.ByteString -> [Header] -> ByteString -> Application -> IO SResponse
doRequestWithQuery method path queryString headers body app = do
    -- Create a request using Network.Wai.Test methods
    let strictBody = BS.concat $ LBS.toChunks body -- Convert from lazy to strict ByteString
    let req = defaultRequest
                { requestMethod = method
                , requestHeaders = headers
                , requestBody = return strictBody
                , rawPathInfo = path  -- Set the path
                , rawQueryString = queryString -- Set the query string
                }
    -- Run the request through the session
    runSession (request req) app

-- Define response type for parsing (updated to include clientId)
data ShortUrlResponseJSON = ShortUrlResponseJSON
    { shortUrlJSON :: Text
    , originalUrlJSON :: Text
    , shortCodeJSON :: Text
    , clientIdJSON :: Maybe Text
    } deriving (Show, Eq)

instance FromJSON ShortUrlResponseJSON where
    parseJSON = withObject "ShortUrlResponseJSON" $ \v -> ShortUrlResponseJSON
        <$> v .: Key.fromString "shortUrl"
        <*> v .: Key.fromString "originalUrl"
        <*> v .: Key.fromString "shortCode"
        <*> v .:? Key.fromString "clientId"

-- Test specifications
spec :: Spec
spec = beforeAll setupApp $ do
    describe "API Integration" $ do
        describe "URL Shortening API" $ do
            it "creates a short URL with client ID" $ \app -> do
                -- Print debug info
                liftIO $ putStrLn "Starting test: creates a short URL with client ID"

                -- Create request to shorten a URL with client ID
                let body = encode $ object
                        [ Key.fromString "longUrl" .= T.pack "https://example.com/test"
                        , Key.fromString "expiresIn" .= (30 :: Int)
                        ]

                liftIO $ putStrLn $ "Request body: " ++ show body

                response <- doRequestWithQuery
                    methodPost
                    (BS.pack "/api/shorten")
                    (BS.pack "?clientId=test-client")
                    [(hContentType, BS.pack "application/json")]
                    body
                    app

                -- Debug info
                liftIO $ putStrLn $ "Response status: " ++ show (simpleStatus response)
                liftIO $ putStrLn $ "Response body: " ++ show (simpleBody response)

                -- Check status and response body
                simpleStatus response `shouldBe` status200
                let respBody = simpleBody response
                respBody `shouldSatisfy` not . LBS.null

                -- Decode response and verify fields
                case decode respBody of
                    Just resp -> do
                        -- Extract shortCode for later tests
                        let shortCode = getShortCode resp
                        shortCode `shouldSatisfy` not . T.null

                        -- Verify client ID is included
                        case getClientId resp of
                            Just clientId -> clientId `shouldBe` T.pack "test-client"
                            Nothing -> expectationFailure "Client ID not found in response"
                    Nothing -> expectationFailure "Could not decode response"

            it "gets URLs for a specific client" $ \app -> do
                -- First, create a short URL for test-client-1
                let createBody1 = encode $ object
                        [ Key.fromString "longUrl" .= T.pack "https://example.com/client1"
                        ]
                createResp1 <- doRequestWithQuery
                    methodPost
                    (BS.pack "/api/shorten")
                    (BS.pack "?clientId=test-client-1")
                    [(hContentType, BS.pack "application/json")]
                    createBody1
                    app

                -- Then, create a short URL for test-client-2
                let createBody2 = encode $ object
                        [ Key.fromString "longUrl" .= T.pack "https://example.com/client2"
                        ]
                createResp2 <- doRequestWithQuery
                    methodPost
                    (BS.pack "/api/shorten")
                    (BS.pack "?clientId=test-client-2")
                    [(hContentType, BS.pack "application/json")]
                    createBody2
                    app

                -- Get URLs for test-client-1
                getResp <- doRequestWithQuery
                    methodGet
                    (BS.pack "/api/urls")
                    (BS.pack "?clientId=test-client-1")
                    []
                    LBS.empty
                    app

                -- Check response
                simpleStatus getResp `shouldBe` status200
                let respBody = simpleBody getResp

                -- Response should be a JSON array with exactly one URL
                case decode respBody of
                    Just (urls :: [Value]) -> do
                        length urls `shouldBe` 1
                        case urls of
                            [url] ->
                                case getOriginalUrl url of
                                    Just originalUrl -> originalUrl `shouldBe` T.pack "https://example.com/client1"
                                    Nothing -> expectationFailure "Original URL not found in response"
                            _ -> expectationFailure "Expected exactly one URL"
                    Nothing -> expectationFailure "Could not decode response as array"

            it "returns URL information with client ID" $ \app -> do
                -- First, create a short URL
                let createBody = encode $ object
                        [ Key.fromString "longUrl" .= T.pack "https://example.com/info"
                        ]
                createResp <- doRequestWithQuery
                    methodPost
                    (BS.pack "/api/shorten")
                    (BS.pack "?clientId=test-client-3")
                    [(hContentType, BS.pack "application/json")]
                    createBody
                    app

                -- Extract shortCode
                let shortCode = extractShortCode $ simpleBody createResp
                    urlPath = BS.append (BS.pack "/api/urls/") (TE.encodeUtf8 shortCode)

                -- Debug info
                liftIO $ putStrLn $ "Extracted short code: " ++ T.unpack shortCode
                liftIO $ putStrLn $ "Request path: " ++ BS.unpack urlPath

                -- Request URL info with client ID
                infoResp <- doRequestWithQuery
                    methodGet
                    urlPath
                    (BS.pack "?clientId=test-client-3")
                    []
                    LBS.empty
                    app

                -- Debug info
                liftIO $ putStrLn $ "Info response status: " ++ show (simpleStatus infoResp)
                liftIO $ putStrLn $ "Info response body: " ++ show (simpleBody infoResp)

                -- Check response
                simpleStatus infoResp `shouldBe` status200
                let respBody = simpleBody infoResp
                case decode respBody of
                    Just resp -> do
                        -- Verify original URL
                        case getOriginalUrl resp of
                            Just url -> url `shouldBe` T.pack "https://example.com/info"
                            Nothing -> expectationFailure "Original URL not found"

                        -- Verify client ID
                        case getClientId resp of
                            Just clientId -> clientId `shouldBe` T.pack "test-client-3"
                            Nothing -> expectationFailure "Client ID not found"
                    Nothing -> expectationFailure "Could not decode info response"

-- Helper functions
getShortCode :: Value -> Text
getShortCode (Object v) =
    fromMaybe
  T.empty (AT.parseMaybe (.: Key.fromString "shortCode") v)
getShortCode _ = T.empty

getOriginalUrl :: Value -> Maybe Text
getOriginalUrl (Object v) =
    AT.parseMaybe (.: Key.fromString "originalUrl") v
getOriginalUrl _ = Nothing

getClientId :: Value -> Maybe Text
getClientId (Object v) =
    AT.parseMaybe (.: Key.fromString "clientId") v
getClientId _ = Nothing

extractShortCode :: ByteString -> Text
extractShortCode respBody =
    case decode respBody of
        Just obj@(Object _) -> getShortCode obj
        _ -> T.empty
