module ModelsSpec (spec) where

import Test.Hspec
import Database.Persist.Sql (runSqlPool, insertEntity, get, delete, entityVal, entityKey, (==.))
import Database.Persist.Postgresql (withPostgresqlPool)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime, addUTCTime, nominalDay)
import Data.Text (Text)
import qualified Data.Text as T

import Models
import Config (loadConfig, AppConfig(..), DatabaseConfig(..))

spec :: Spec
spec = around withTestDB $ do
  describe "Database Models" $ do
    describe "ShortUrl" $ do
      it "can be created and retrieved" $ \pool -> do
        now <- liftIO getCurrentTime
        let expiryTime = addUTCTime (30 * nominalDay) now
            shortUrl = ShortUrl "https://example.com" "abc123" now (Just expiryTime) 0
        
        -- Insert and retrieve
        entityId <- liftIO $ runSqlPool (insertEntity shortUrl) pool
        retrieved <- liftIO $ runSqlPool (get $ entityKey entityId) pool
        
        -- Verify
        retrieved `shouldBe` Just shortUrl
        
      it "can have its click count updated" $ \pool -> do
        now <- liftIO getCurrentTime
        let expiryTime = addUTCTime (30 * nominalDay) now
            shortUrl = ShortUrl "https://example.com" "click123" now (Just expiryTime) 0
        
        -- Insert
        entityId <- liftIO $ runSqlPool (insertEntity shortUrl) pool
        let key = entityKey entityId
        
        -- Update click count
        liftIO $ runSqlPool (updateClickCount key) pool
        
        -- Retrieve and verify
        retrieved <- liftIO $ runSqlPool (get key) pool
        case retrieved of
          Just url -> shortUrlClickCount url `shouldBe` 1
          Nothing -> expectationFailure "ShortUrl not found after updating click count"
        
      it "enforces unique short codes" $ \pool -> do
        now <- liftIO getCurrentTime
        let url1 = ShortUrl "https://example.com/1" "unique123" now Nothing 0
            url2 = ShortUrl "https://example.com/2" "unique123" now Nothing 0
        
        -- Insert first URL
        _ <- liftIO $ runSqlPool (insertEntity url1) pool
        
        -- Attempt to insert second URL with same short code
        result <- liftIO $ try $ runSqlPool (insertEntity url2) pool
        case result of
          Left (e :: SomeException) -> return () -- Expected exception
          Right _ -> expectationFailure "Should have failed with unique constraint violation"

-- Helper function for updating click count
updateClickCount :: ShortUrlId -> SqlPersistM ()
updateClickCount urlId = do
  update urlId [ShortUrlClickCount +=. 1]

-- Set up a test database connection
withTestDB :: (Pool SqlBackend -> IO a) -> IO a
withTestDB action = do
  config <- loadConfig
  let dbConfig = Config.dbConfig config
      connStr = createConnectionString dbConfig
  
  runStdoutLoggingT $ withPostgresqlPool connStr 1 $ \pool -> do
    -- Run migrations
    runSqlPool (runMigration migrateAll) pool
    
    -- Clean database before test
    runSqlPool (deleteWhere ([] :: [Filter ShortUrl])) pool
    
    -- Run the test
    liftIO $ action pool

-- Create a PostgreSQL connection string
createConnectionString :: DatabaseConfig -> ConnectionString
createConnectionString DatabaseConfig{..} =
  encodeUtf8 $ T.concat 
    [ "host=", dbHost
    , " port=", T.pack (show dbPort)
    , " user=", dbUser
    , " password=", dbPassword
    , " dbname=", dbName
    ]

