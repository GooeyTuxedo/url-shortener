module IntegrationTests exposing (suite)

import Expect
import Http
import Main.Internal exposing (update)
import Msg exposing (Msg(..))
import Test exposing (..)
import TestHelpers exposing (mockModel, mockShortUrl)
import Types exposing (..)
import Url


suite : Test
suite =
    describe "Integration Tests"
        [ describe "User Journey"
            [ test "Complete URL shortening flow" <|
                \_ ->
                    let
                        -- Start with initial model
                        initialModel = mockModel

                        -- 1. User enters a URL
                        ( model1, _ ) =
                            update (UrlInputChanged "https://example.com/long/url") initialModel

                        -- 2. User enables custom alias
                        ( model2, _ ) =
                            update (ToggleCustomAlias True) model1

                        -- 3. User enters custom alias
                        ( model3, _ ) =
                            update (CustomAliasInputChanged "myalias") model2

                        -- 4. User selects expiry
                        ( model4, _ ) =
                            update (ExpiresInChanged (Just 7)) model3

                        -- 5. User submits form - this would normally send an HTTP request
                        ( model5, _ ) =
                            update SubmitForm model4

                        -- 6. Simulate API response
                        ( finalModel, _ ) =
                            update (UrlCreated (Ok mockShortUrl)) model5
                    in
                    -- Assert the final state
                    Expect.all
                        [ \m -> Expect.notEqual m.shortenForm model4.shortenForm -- Form should be reset
                        , \m -> Expect.equal (List.length m.urls) 1 -- URL should be added to list
                        , \m -> Expect.equal m.currentUrl (Just mockShortUrl) -- Current URL should be set
                        , \m -> 
                            case m.notification of
                                Just notification ->
                                    Expect.equal notification.notificationType Success -- Should show success
                                Nothing ->
                                    Expect.fail "Expected success notification"
                        ]
                        finalModel
            ]
        , describe "Navigation"
            [ test "Navigation to URL Details page" <|
                \_ ->
                    let
                        -- Start with initial model that has some URLs
                        initialModel = 
                            { mockModel 
                                | urls = [ mockShortUrl ]
                                , currentUrl = Just mockShortUrl
                            }

                        -- 1. User navigates to URL details
                        url =
                            { protocol = Url.Http
                            , host = "localhost"
                            , port_ = Just 1234
                            , path = "/url/abc123"
                            , query = Nothing
                            , fragment = Nothing
                            }
                            
                        -- Simulate URL change
                        ( model1, _ ) =
                            update (UrlChanged url) initialModel
                            
                        -- Simulate URL details loading
                        ( finalModel, _ ) =
                            update (UrlDetailsLoaded (Ok mockShortUrl)) model1
                    in
                    -- Assert the final state shows the URL details page
                    Expect.equal finalModel.page (UrlDetailsPage "abc123")
            ]
        , describe "Error Handling"
            [ test "Form validation shows error for empty URL" <|
                \_ ->
                    let
                        -- Start with initial model with empty URL
                        initialModel = mockModel
                        
                        -- Submit form with empty URL
                        ( finalModel, _ ) =
                            update SubmitForm initialModel
                    in
                    -- Should show error notification
                    case finalModel.notification of
                        Just notification ->
                            Expect.equal notification.notificationType Error
                        Nothing ->
                            Expect.fail "Expected error notification"
                            
            , test "API error handling shows error notification" <|
                \_ ->
                    let
                        -- Start with initial model
                        initialModel = { mockModel | isLoading = True }
                        
                        -- Simulate API error
                        ( finalModel, _ ) =
                            update (UrlCreated (Err (Http.BadStatus 409))) initialModel
                    in
                    -- Should show error notification with correct message
                    case finalModel.notification of
                        Just notification ->
                            Expect.all
                                [ \n -> Expect.equal n.notificationType Error
                                , \n -> Expect.equal n.message "Custom alias already in use"
                                ]
                                notification
                        Nothing ->
                            Expect.fail "Expected error notification"
            ]
        , describe "State management"
            [ test "URL list is preserved when navigating" <|
                \_ ->
                    let
                        -- Start with model having URLs
                        initialModel = 
                            { mockModel 
                                | urls = [ mockShortUrl ]
                            }
                        
                        -- Navigate to details page
                        url1 =
                            { protocol = Url.Http
                            , host = "localhost"
                            , port_ = Just 1234
                            , path = "/url/abc123"
                            , query = Nothing
                            , fragment = Nothing
                            }
                            
                        ( model1, _ ) =
                            update (UrlChanged url1) initialModel
                            
                        -- Navigate back to home
                        url2 =
                            { protocol = Url.Http
                            , host = "localhost"
                            , port_ = Just 1234
                            , path = "/"
                            , query = Nothing
                            , fragment = Nothing
                            }
                            
                        ( finalModel, _ ) =
                            update (UrlChanged url2) model1
                    in
                    -- URLs should be preserved
                    Expect.equal (List.length finalModel.urls) 1
            ]
        ]