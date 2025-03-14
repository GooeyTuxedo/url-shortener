module ViewTests exposing (suite)

import Element exposing (Element, layout)
import Html
import Msg exposing (Msg(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import TestableNavigation
import Time
import Types exposing (..)
import View.Home
import View.Layout
import View.NotFound
import View.Shortener
import View.Shortener.Internal
import View.UrlDetails


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


suite : Test
suite =
    describe "View Modules"
        [ describe "Home View"
            [ test "Home view includes hero section" <|
                \_ ->
                    View.Home.viewHome mockModel
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Shorten Your Links" ]
            , test "Home view with no URLs shows empty state" <|
                \_ ->
                    View.Home.viewHome mockModel
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "No URLs yet" ]
            , test "Home view with URLs shows URL list" <|
                \_ ->
                    let
                        modelWithUrls =
                            { mockModel | urls = [ mockShortUrl ] }
                    in
                    View.Home.viewHome modelWithUrls
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Your URLs" ]
            ]
        , describe "NotFound View"
            [ test "NotFound view shows 404" <|
                \_ ->
                    View.NotFound.viewNotFound mockModel
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "404" ]
            , test "NotFound view has link back to home" <|
                \_ ->
                    View.NotFound.viewNotFound mockModel
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Back to Home" ]
            ]
        , describe "Shortener View"
            [ test "Shortener form has URL input field" <|
                \_ ->
                    View.Shortener.Internal.viewShortenForm mockModel.shortenForm False
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Long URL" ]
            , test "Shortener form has custom alias option" <|
                \_ ->
                    View.Shortener.Internal.viewShortenForm mockModel.shortenForm False
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Use custom alias" ]
            , test "Shortener form has expiration option" <|
                \_ ->
                    View.Shortener.Internal.viewShortenForm mockModel.shortenForm False
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Expires In" ]
            , test "Shortener result shows short URL" <|
                \_ ->
                    View.Shortener.viewResult mockShortUrl
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Your Short URL" ]
            ]
        , describe "URL Details View"
            [ test "URL Details view shows loading when no URL is loaded" <|
                \_ ->
                    View.UrlDetails.viewUrlDetails { mockModel | isLoading = True } "testcode"
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Loading..." ]
            , test "URL Details view shows not found when URL doesn't exist" <|
                \_ ->
                    View.UrlDetails.viewUrlDetails mockModel "testcode"
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "URL Not Found" ]
            , test "URL Details view shows details when URL exists" <|
                \_ ->
                    let
                        modelWithUrl =
                            { mockModel | currentUrl = Just mockShortUrl }
                    in
                    View.UrlDetails.viewUrlDetails modelWithUrl mockShortUrl.shortCode
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "URL Details" ]
            ]
        , describe "Layout"
            [ test "Layout includes header" <|
                \_ ->
                    View.Layout.viewHeader mockModel
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "URL Shortener" ]
            , test "Layout includes footer" <|
                \_ ->
                    View.Layout.viewFooter
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "© 2025 URL Shortener" ]
            , test "Layout shows notification when present" <|
                \_ ->
                    let
                        notification =
                            { message = "Test notification", notificationType = Info }
                    in
                    View.Layout.viewNotification notification
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Test notification" ]
            ]
        , describe "Client ID features"
            [ test "Layout shows client ID badge when ID is set" <|
                \_ ->
                    let
                        modelWithClientId = { mockModel | clientId = Just "test-client" }
                    in
                    View.Layout.viewHeader modelWithClientId
                        |> elementToHtml
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "ID: test-client" ]
            ]
        ]
