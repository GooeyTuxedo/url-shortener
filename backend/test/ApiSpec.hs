{-# LANGUAGE OverloadedStrings #-}

module ApiSpec (spec) where

import Test.Hspec
import Network.Wai.Test (Session, SResponse(..), request, runSession, defaultRequest, simpleHeaders)
import Network.Wai.Internal (requestMethod, requestHeaders, requestBody, rawPathInfo)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Header (HeaderName, hContentType)
import qualified Data.CaseInsensitive as CI
import Network.Wai (Application)
import Network.HTTP.Types (methodGet, methodPost, status200, status301, status404, Header)
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), encode, decode, object, withObject, (.=), (.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AT
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
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

-- Define response type for parsing
data ShortUrlResponseJSON = ShortUrlResponseJSON
    { shortUrlJSON :: Text
    , originalUrlJSON :: Text
    , shortCodeJSON :: Text
    } deriving (Show, Eq)

instance FromJSON ShortUrlResponseJSON where
    parseJSON = withObject "ShortUrlResponseJSON" $ \v -> ShortUrlResponseJSON
        <$> v .: Key.fromString "shortUrl"
        <*> v .: Key.fromString "originalUrl"
        <*> v .: Key.fromString "shortCode"

-- Test specifications
spec :: Spec
spec = beforeAll setupApp $ do
    describe "API Integration" $ do
        describe "URL Shortening API" $ do
            it "creates a short URL" $ \app -> do
                -- Print debug info
                liftIO $ putStrLn "Starting test: creates a short URL"
            
                -- Create request to shorten a URL
                let body = encode $ object
                        [ Key.fromString "longUrl" .= T.pack "https://example.com/test"
                        , Key.fromString "expiresIn" .= (30 :: Int)
                        ]
                
                liftIO $ putStrLn $ "Request body: " ++ show body
                
                response <- doRequest methodPost "/api/shorten" [(hContentType, "application/json")] body app
                
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
                    Nothing -> expectationFailure "Could not decode response"
                    
            it "returns URL information by shortCode" $ \app -> do
                -- First, create a short URL
                let createBody = encode $ object
                        [ Key.fromString "longUrl" .= T.pack "https://example.com/info"
                        ]
                createResp <- doRequest methodPost "/api/shorten" [(hContentType, "application/json")] createBody app
                
                -- Extract shortCode
                let shortCode = extractShortCode $ simpleBody createResp
                    urlPath = "/api/urls/" <> TE.encodeUtf8 shortCode
                
                -- Debug info
                liftIO $ putStrLn $ "Extracted short code: " ++ T.unpack shortCode
                liftIO $ putStrLn $ "Request path: " ++ BS.unpack urlPath
                
                -- Request URL info
                infoResp <- doRequest methodGet urlPath [] LBS.empty app
                
                -- Debug info
                liftIO $ putStrLn $ "Info response status: " ++ show (simpleStatus infoResp)
                liftIO $ putStrLn $ "Info response body: " ++ show (simpleBody infoResp)
                
                -- Check response
                simpleStatus infoResp `shouldBe` status200
                let respBody = simpleBody infoResp
                case decode respBody of
                    Just resp -> do
                        getOriginalUrl resp `shouldBe` T.pack "https://example.com/info"
                    Nothing -> expectationFailure "Could not decode info response"
                
            it "redirects to original URL" $ \app -> do
                -- Create a short URL
                let createBody = encode $ object
                        [ Key.fromString "longUrl" .= T.pack "https://example.com/redirect"
                        ]
                createResp <- doRequest methodPost "/api/shorten" [(hContentType, "application/json")] createBody app
                
                -- Extract shortCode
                let shortCode = extractShortCode $ simpleBody createResp
                    redirectPath = "/" <> TE.encodeUtf8 shortCode
                
                -- Debug info
                liftIO $ putStrLn $ "Redirect test - short code: " ++ T.unpack shortCode
                liftIO $ putStrLn $ "Redirect path: " ++ BS.unpack redirectPath
                
                -- Request redirect
                redirectResp <- doRequest methodGet redirectPath [] LBS.empty app
                
                -- Debug info
                liftIO $ putStrLn $ "Redirect response status: " ++ show (simpleStatus redirectResp)
                liftIO $ putStrLn $ "Redirect headers: " ++ show (simpleHeaders redirectResp)
                
                -- Check redirect status and Location header
                simpleStatus redirectResp `shouldBe` status301
                -- Convert CI HeaderName to BS.ByteString for lookups
                let headers = map (\(n, v) -> (CI.original n, v)) (simpleHeaders redirectResp)
                lookup (BS.pack "Location") headers `shouldBe` Just (BS.pack "https://example.com/redirect")
        
            it "returns 404 for non-existent shortCode" $ \app -> do
                -- Request URL info for non-existent shortCode
                response <- doRequest methodGet "/api/urls/nonexistent" [] LBS.empty app
                
                -- Check status
                simpleStatus response `shouldBe` status404

-- Helper functions
getShortCode :: Value -> Text
getShortCode (Object v) = 
    case AT.parseMaybe (.: Key.fromString "shortCode") v of
        Just s -> s
        Nothing -> T.empty
getShortCode _ = T.empty

getOriginalUrl :: Value -> Text
getOriginalUrl (Object v) = 
    case AT.parseMaybe (.: Key.fromString "originalUrl") v of
        Just s -> s
        Nothing -> T.empty
getOriginalUrl _ = T.empty

extractShortCode :: ByteString -> Text
extractShortCode respBody =
    case decode respBody of
        Just obj@(Object _) -> getShortCode obj
        _ -> T.empty
