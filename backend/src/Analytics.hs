{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Analytics
    ( recordClick
    , ClickData(..)
    , ClickStats(..)
    , getUrlStats
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Ord (Down(Down))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (TimeZone, hoursToTimeZone, todHour, utcToLocalTime, localTimeOfDay)
import Database.Persist ((==.), Entity(..), insert, selectList)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Database.Persist.TH
import GHC.Generics (Generic)
import IPUtils (ClientIP(..), getClientIP)
import Models (ShortUrl, ShortUrlId)
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request, requestHeaders)
import Data.List (sortBy)

-- Click data for analytics
share [mkPersist sqlSettings] [persistLowerCase|
ClickEvent
    shortUrlId ShortUrlId
    clickTime UTCTime
    ipAddress Text
    userAgent Text Maybe
    referer Text Maybe
    country Text Maybe
    device Text Maybe
    browser Text Maybe
    deriving Show Eq
|]

-- Statistics for a URL
data ClickStats = ClickStats
    { totalClicks :: Int
    , uniqueVisitors :: Int
    , clicksByCountry :: Map Text Int
    , clicksByReferer :: Map Text Int
    , clicksByBrowser :: Map Text Int
    , clicksByDevice :: Map Text Int
    , clicksByHour :: Map Int Int
    , recentClicks :: [ClickData]
    } deriving (Show, Eq, Generic)

instance ToJSON ClickStats
instance FromJSON ClickStats

-- Click data for API responses
data ClickData = ClickData
    { clickTime :: UTCTime
    , ipAddress :: Text
    , userAgent :: Maybe Text
    , referer :: Maybe Text
    , country :: Maybe Text
    , device :: Maybe Text
    , browser :: Maybe Text
    } deriving (Show, Eq, Generic)

instance ToJSON ClickData
instance FromJSON ClickData

-- Record a click event
recordClick :: Pool SqlBackend -> ShortUrlId -> Request -> IO ClickEventId
recordClick pool shortUrlId req = do
    now <- getCurrentTime

    let clientIP = unClientIP $ getClientIP req
        userAgent = getHeader "User-Agent" req
        referer = getHeader "Referer" req

        -- These would normally be determined by IP geolocation and user agent parsing
        country = Just "Unknown"
        device = Just "Unknown"
        browser = Just "Unknown"

    -- Create the ClickEvent
    let clickEvent = ClickEvent
            { clickEventShortUrlId = shortUrlId
            , clickEventClickTime = now
            , clickEventIpAddress = clientIP
            , clickEventUserAgent = userAgent
            , clickEventReferer = referer
            , clickEventCountry = country
            , clickEventDevice = device
            , clickEventBrowser = browser
            }

    -- Run the insert operation inside runSqlPool
    runSqlPool (insert clickEvent) pool

-- Get statistics for a URL
getUrlStats :: Pool SqlBackend -> ShortUrlId -> IO ClickStats
getUrlStats pool shortUrlId = do
    -- Get all click events for this URL using runSqlPool
    clicks <- runSqlPool (selectList [ClickEventShortUrlId ==. shortUrlId] []) pool

    let clickEntities = map entityVal clicks

        -- Calculate basic stats
        total = length clickEntities
        uniqueIPs = length $ Map.fromList $
            map (\c -> (clickEventIpAddress c, ())) clickEntities

        -- Group clicks by various dimensions
        byCountry = countByField clickEventCountry clickEntities
        byReferer = countByField clickEventReferer clickEntities
        byBrowser = countByField clickEventBrowser clickEntities
        byDevice = countByField clickEventDevice clickEntities
        byHour = countByHour clickEntities

        -- Most recent clicks (up to 100)
        recentClicksData = map toClickData $ take 100 $
            sortOn (Down . clickEventClickTime) clickEntities

    return $ ClickStats
        { totalClicks = total
        , uniqueVisitors = uniqueIPs
        , clicksByCountry = byCountry
        , clicksByReferer = byReferer
        , clicksByBrowser = byBrowser
        , clicksByDevice = byDevice
        , clicksByHour = byHour
        , recentClicks = recentClicksData
        }

-- Helper: Get a header value from the request
getHeader :: HeaderName -> Request -> Maybe Text
getHeader name req =
    case lookup name (requestHeaders req) of
        Just val -> Just $ TE.decodeUtf8 val
        Nothing -> Nothing

-- Helper: Count occurrences by a field
countByField :: (Ord a) => (ClickEvent -> Maybe a) -> [ClickEvent] -> Map a Int
countByField field = foldr countField Map.empty
  where
    countField event acc =
        case field event of
            Just val -> Map.insertWith (+) val 1 acc
            Nothing -> acc

-- Helper: Count clicks by hour of day (0-23)
countByHour :: [ClickEvent] -> Map Int Int
countByHour = foldr countHour Map.empty
  where
    countHour event acc =
        -- Use a simple conversion to local time (assuming UTC)
        let localTime = utcToLocalTime (hoursToTimeZone 0) (clickEventClickTime event)
            hour = todHour $ localTimeOfDay localTime
        in Map.insertWith (+) hour 1 acc

-- Helper: Sort a list by a field
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))

-- Helper: Compare values
comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing f x y = compare (f x) (f y)

-- Helper: Convert ClickEvent to ClickData
toClickData :: ClickEvent -> ClickData
toClickData event = ClickData
    { clickTime = clickEventClickTime event
    , ipAddress = clickEventIpAddress event
    , userAgent = clickEventUserAgent event
    , referer = clickEventReferer event
    , country = clickEventCountry event
    , device = clickEventDevice event
    , browser = clickEventBrowser event
    }
