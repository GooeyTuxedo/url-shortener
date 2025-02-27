module MainTests exposing (suite)

import Expect
import Http
import Main.Internal exposing (update)
import Msg exposing (Msg(..))
import Test exposing (..)
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
    }


suite : Test
suite =
    describe "Main Module"
        [ describe "update function"
            [ test "UrlInputChanged updates shortenForm.url" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (UrlInputChanged "https://test.com") mockModel
                    in
                    Expect.equal newModel.shortenForm.url "https://test.com"
            , test "CustomAliasInputChanged updates shortenForm.customAlias" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (CustomAliasInputChanged "myalias") mockModel
                    in
                    Expect.equal newModel.shortenForm.customAlias "myalias"
            , test "ToggleCustomAlias updates shortenForm.useCustomAlias" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (ToggleCustomAlias True) mockModel
                    in
                    Expect.equal newModel.shortenForm.useCustomAlias True
            , test "ExpiresInChanged updates shortenForm.expiresIn" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (ExpiresInChanged (Just 7)) mockModel
                    in
                    Expect.equal newModel.shortenForm.expiresIn (Just 7)
            , test "SubmitForm with empty URL shows error notification" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update SubmitForm mockModel
                    in
                    case newModel.notification of
                        Just notification ->
                            Expect.equal notification.notificationType Error
                        Nothing ->
                            Expect.fail "Expected notification to be shown"
            , test "WindowResized updates windowWidth" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (WindowResized 800) mockModel
                    in
                    Expect.equal newModel.windowWidth 800
            , test "UrlCreated with Ok result updates model correctly" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (UrlCreated (Ok mockShortUrl)) mockModel
                    in
                    Expect.all
                        [ \m -> Expect.equal (List.length m.urls) 1
                        , \m -> Expect.equal m.currentUrl (Just mockShortUrl)
                        , \m -> Expect.equal m.isLoading False
                        , \m -> 
                            case m.notification of
                                Just notification ->
                                    Expect.equal notification.notificationType Success
                                Nothing ->
                                    Expect.fail "Expected success notification"
                        ]
                        newModel
            , test "UrlCreated with Err result shows error notification" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (UrlCreated (Err (Http.BadStatus 400))) mockModel
                    in
                    Expect.all
                        [ \m -> Expect.equal m.isLoading False
                        , \m -> 
                            case m.notification of
                                Just notification ->
                                    Expect.equal notification.notificationType Error
                                Nothing ->
                                    Expect.fail "Expected error notification"
                        ]
                        newModel
            , test "DismissNotification removes notification" <|
                \_ ->
                    let
                        modelWithNotification =
                            { mockModel | notification = Just { message = "Test", notificationType = Info } }
                        
                        ( newModel, _ ) =
                            update DismissNotification modelWithNotification
                    in
                    Expect.equal newModel.notification Nothing
            , test "ShowNotification adds notification" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (ShowNotification "Test Message" Warning) mockModel
                    in
                    case newModel.notification of
                        Just notification ->
                            Expect.all
                                [ \n -> Expect.equal n.message "Test Message"
                                , \n -> Expect.equal n.notificationType Warning
                                ]
                                notification
                        Nothing ->
                            Expect.fail "Expected notification to be shown"
            ]
        ]