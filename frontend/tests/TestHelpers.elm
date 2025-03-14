module TestHelpers exposing
    ( mockModel
    , mockModelWithData
    , mockShortUrl
    , mockShortenForm
    , elementToHtml
    , mockApiResponse
    )

import Element exposing (Element, layout)
import Html
import Http
import Msg exposing (Msg)
import TestableNavigation
import Time
import Types exposing (..)


-- Create a mock model for testing
mockModel : Model
mockModel =
    { apiUrl = "http://test-api.com"
    , windowWidth = 1200
    , page = HomePage
    , urls = []
    , currentUrl = Nothing
    , shortenForm =
        { url = ""
        , customAlias = ""
        , useCustomAlias = False
        , expiresIn = Nothing
        }
    , notification = Nothing
    , isLoading = False
    , errorMessage = Nothing
    , navKey = TestableNavigation.dummy
    , clientId = Nothing
    , tempClientId = ""
    }


-- Create a mock ShortUrl for testing
mockShortUrl : ShortUrl
mockShortUrl =
    { shortUrl = "http://short.url/abc123"
    , originalUrl = "http://example.com/long/url"
    , shortCode = "abc123"
    , createdAt = Time.millisToPosix 0
    , expiresAt = Nothing
    , clickCount = 0
    , qrCodeUrl = "http://api.example.com/qr/abc123"
    , clientId = "test-client"
    }


-- Helper function to render Element to Html for testing
elementToHtml : Element Msg -> Html.Html Msg
elementToHtml element =
    layout [] element


-- Helper to create mock HTTP responses
mockApiResponse : Result Http.Error a -> (Result Http.Error a -> msg) -> msg
mockApiResponse result toMsg =
    toMsg result


-- Add more ShortUrl examples with different properties
mockShortUrlWithExpiry : ShortUrl
mockShortUrlWithExpiry =
    { mockShortUrl
        | expiresAt = Just (Time.millisToPosix 1614556800000) -- Some future date
    }


mockShortUrlWithClicks : ShortUrl
mockShortUrlWithClicks =
    { mockShortUrl
        | clickCount = 42
    }


-- Create a model with sample data for more extensive testing
mockModelWithData : Model
mockModelWithData =
    { mockModel
        | urls =
            [ mockShortUrl
            , mockShortUrlWithExpiry
            , mockShortUrlWithClicks
            ]
        , currentUrl = Just mockShortUrl
        , clientId = Just "test-client"
    }


-- Mock form data
mockShortenForm : ShortenForm
mockShortenForm =
    { url = "https://example.com/very/long/url/that/needs/shortening"
    , customAlias = "testAlias"
    , useCustomAlias = True
    , expiresIn = Just 30
    }