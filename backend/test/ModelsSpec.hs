{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ModelsSpec (spec) where

import Test.Hspec
import Database.Persist.Sql (runSqlPool, insertEntity, get, delete, entityVal, entityKey, (==.),
                           update, (+=.), Entity, Filter, deleteWhere, SqlPersistT, ConnectionPool,
                           SqlBackend, runMigration, selectList)
import Database.Persist.Postgresql (withPostgresqlPool, ConnectionString)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay, diffUTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (encodeUtf8)
import Control.Exception (try, SomeException)

import Models

import Config (loadConfig, AppConfig(..), DatabaseConfig(..), dbConfig)

-- Helper to compare ShortUrls ignoring microseconds in timestamps
shortUrlEqual :: ShortUrl -> ShortUrl -> Bool
shortUrlEqual a b =
  shortUrlOriginalUrl a == shortUrlOriginalUrl b &&
  shortUrlShortCode a == shortUrlShortCode b &&
  shortUrlClickCount a == shortUrlClickCount b &&
  shortUrlClientId a == shortUrlClientId b &&  -- Check client ID
    -- Check client ID
  (abs (diffUTCTime (shortUrlCreatedAt a) (shortUrlCreatedAt b)) < 1) &&
  case (shortUrlExpiresAt a, shortUrlExpiresAt b) of
    (Just ta, Just tb) -> abs (diffUTCTime ta tb) < 1
    (Nothing, Nothing) -> True
    _ -> False

spec :: Spec
spec = around withTestDB $ do
  describe "Database Models" $ do
    describe "ShortUrl" $ do
      it "can be created and retrieved with client ID" $ \pool -> do
        now <- liftIO getCurrentTime
        let expiryTime = addUTCTime (30 * nominalDay) now
            shortUrl = ShortUrl "https://example.com" "abc123" now (Just expiryTime) 0 "test-client"  -- Added client ID

        -- Insert and retrieve
        entityId <- liftIO $ runSqlPool (insertEntity shortUrl) pool
        retrieved <- liftIO $ runSqlPool (get $ entityKey entityId) pool

        -- Verify using custom comparison that ignores microsecond precision differences
        case retrieved of
          Just retrievedUrl -> do
            shortUrlEqual shortUrl retrievedUrl `shouldBe` True
            shortUrlClientId retrievedUrl `shouldBe` "test-client"  -- Explicitly check client ID
          Nothing -> expectationFailure "Failed to retrieve ShortUrl"

      it "can have its click count updated" $ \pool -> do
        now <- liftIO getCurrentTime
        let expiryTime = addUTCTime (30 * nominalDay) now
            shortUrl = ShortUrl "https://example.com" "click123" now (Just expiryTime) 0 "test-client"  -- Added client ID

        -- Insert
        entityId <- liftIO $ runSqlPool (insertEntity shortUrl) pool
        let key = entityKey entityId

        -- Update click count
        liftIO $ runSqlPool (updateClickCount key) pool

        -- Retrieve and verify
        retrieved <- liftIO $ runSqlPool (get key) pool
        case retrieved of
          Just url -> do
            shortUrlClickCount url `shouldBe` 1
            shortUrlClientId url `shouldBe` "test-client"  -- Client ID should remain unchanged
          Nothing -> expectationFailure "ShortUrl not found after updating click count"

      it "can filter URLs by client ID" $ \pool -> do
        now <- liftIO getCurrentTime

        -- Create URLs for different clients
        let url1 = ShortUrl "https://example.com/1" "client1-url" now Nothing 0 "client1"
            url2 = ShortUrl "https://example.com/2" "client2-url" now Nothing 0 "client2"
            url3 = ShortUrl "https://example.com/3" "client1-url2" now Nothing 0 "client1"

        -- Insert all URLs
        _ <- liftIO $ runSqlPool (insertEntity url1) pool
        _ <- liftIO $ runSqlPool (insertEntity url2) pool
        _ <- liftIO $ runSqlPool (insertEntity url3) pool

        -- Query URLs for client1
        client1Urls <- liftIO $ runSqlPool (selectList [ShortUrlClientId ==. "client1"] []) pool

        -- Verify we get the right number of results
        length client1Urls `shouldBe` 2

        -- Client2 should only have one URL
        client2Urls <- liftIO $ runSqlPool (selectList [ShortUrlClientId ==. "client2"] []) pool
        length client2Urls `shouldBe` 1

      it "enforces unique short codes across clients" $ \pool -> do
        now <- liftIO getCurrentTime
        let url1 = ShortUrl "https://example.com/1" "unique123" now Nothing 0 "client1"
            url2 = ShortUrl "https://example.com/2" "unique123" now Nothing 0 "client2"  -- Same code, different client

        -- Insert first URL
        _ <- liftIO $ runSqlPool (insertEntity url1) pool

        -- Attempt to insert second URL with same short code (should fail even with different client)
        result <- liftIO $ try (runSqlPool (insertEntity url2) pool) :: IO (Either SomeException (Entity ShortUrl))
        case result of
          Left (e :: SomeException) -> return () -- Expected exception
          Right _ -> expectationFailure "Should have failed with unique constraint violation"

-- Helper function for updating click count
updateClickCount :: ShortUrlId -> SqlPersistT IO ()
updateClickCount urlId = do
  update urlId [ShortUrlClickCount +=. 1]

-- Set up a test database connection
withTestDB :: (ConnectionPool -> IO a) -> IO a
withTestDB action = do
  config <- loadConfig
  let dbConf = dbConfig config
      connStr = createConnectionString dbConf

  runStdoutLoggingT $ withPostgresqlPool connStr 1 $ \pool -> do
    -- Run migrations
    runSqlPool (runMigration migrateAll) pool

    -- Clean database before test
    runSqlPool (deleteWhere ([] :: [Filter ShortUrl])) pool

    -- Run the test
    liftIO $ action pool

-- Create a PostgreSQL connection string
createConnectionString :: DatabaseConfig -> ConnectionString
createConnectionString dbConf =
  TE.encodeUtf8 $ T.concat
    [ "host=", dbHost dbConf
    , " port=", T.pack (show $ dbPort dbConf)
    , " user=", dbUser dbConf
    , " password=", dbPassword dbConf
    , " dbname=", dbName dbConf
    ]
