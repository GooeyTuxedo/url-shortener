module ApiSpec (spec) where

import Test.Hspec
import Network.Wai.Test (Session, runSession, request, simpleStatus, simpleBody)
import Network.Wai (Application)
import Network.HTTP.Types (methodGet, methodPost, status200, status301, status404)
import Data.Aeson (encode, decode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Config (loadConfig)
import App (initConnectionPool, runMigrations)
import Api (apiHandler)
import AppEnv
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import AbuseProtection (BlacklistConfig(..), newUrlContentFilter)
import RateLimiter (RateLimitConfig(..), newRateLimiter)
import qualified Data.Set as Set
import Models (ShortUrl, ShortUrlId)
import Database.Persist.Sql (runSqlPool, deleteWhere, (==.))

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
    runSqlPool (deleteWhere []) pool
    
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

-- Test specifications
spec :: Spec
spec = beforeAll setupApp $ do
    describe "API Integration" $ do
        describe "URL Shortening API" $ do
            it "creates a short URL" $ \app -> do
                -- Create request to shorten a URL
                let body = encode $ object
                        [ "longUrl" .= ("https://example.com/test" :: Text)
                        , "expiresIn" .= (30 :: Int)
                        ]
                response <- runSession (request methodPost "/api/shorten" [] body) app
                
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
                        [ "longUrl" .= ("https://example.com/info" :: Text)
                        ]
                createResp <- runSession (request methodPost "/api/shorten" [] createBody) app
                
                -- Extract shortCode
                let shortCode = extractShortCode $ simpleBody createResp
                
                -- Request URL info
                infoResp <- runSession (request methodGet ("/api/urls/" <> BS.pack (T.unpack shortCode)) [] "") app
                
                -- Check response
                simpleStatus infoResp `shouldBe` status200
                let respBody = simpleBody infoResp
                case decode respBody of
                    Just resp -> do
                        getOriginalUrl resp `shouldBe` "https://example.com/info"
                    Nothing -> expectationFailure "Could not decode info response"
                
            it "redirects to original URL" $ \app -> do
                -- Create a short URL
                let createBody = encode $ object
                        [ "longUrl" .= ("https://example.com/redirect" :: Text)
                        ]
                createResp <- runSession (request methodPost "/api/shorten" [] createBody) app
                
                -- Extract shortCode
                let shortCode = extractShortCode $ simpleBody createResp
                
                -- Request redirect
                redirectResp <- runSession (request methodGet ("/" <> BS.pack (T.unpack shortCode)) [] "") app
                
                -- Check redirect status and Location header
                simpleStatus redirectResp `shouldBe` status301
                let headers = getResponseHeaders redirectResp
                lookup "Location" headers `shouldBe` Just "https://example.com/redirect"
        
            it "returns 404 for non-existent shortCode" $ \app -> do
                -- Request URL info for non-existent shortCode
                response <- runSession (request methodGet "/api/urls/nonexistent" [] "") app
                
                -- Check status
                simpleStatus response `shouldBe` status404

-- Helper functions
getShortCode :: Value -> Text
getShortCode = undefined -- Extract shortCode from response JSON

getOriginalUrl :: Value -> Text
getOriginalUrl = undefined -- Extract originalUrl from response JSON

extractShortCode :: ByteString -> Text
extractShortCode = undefined -- Extract shortCode from response body

getResponseHeaders :: Response -> [(BS.ByteString, BS.ByteString)]
getResponseHeaders = undefined -- Extract headers from response

