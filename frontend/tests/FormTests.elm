module FormTests exposing (suite)

import Expect
import Main.Internal exposing (update)
import Msg exposing (Msg(..))
import Test exposing (..)
import TestHelpers exposing (mockModel, mockShortUrl)
import Types exposing (..)


suite : Test
suite =
    describe "Form Handling"
        [ describe "URL Form Validation"
            [ test "Empty URL shows validation error" <|
                \_ ->
                    let
                        initialModel = mockModel

                        ( model, _ ) =
                            update SubmitForm initialModel
                    in
                    -- Should show error notification
                    case model.notification of
                        Just notification ->
                            Expect.equal notification.message "Please enter a URL"

                        Nothing ->
                            Expect.fail "Expected validation error notification"

            , test "Valid URL form is accepted" <|
                \_ ->
                    let
                        modelWithUrl =
                            { mockModel | shortenForm = { url = "https://example.com", customAlias = "", useCustomAlias = False, expiresIn = Nothing } }

                        ( model, _ ) =
                            update SubmitForm modelWithUrl
                    in
                    -- Should start loading (no validation error)
                    Expect.equal model.isLoading True
            ]

        , describe "Form Input Handling"
            [ test "URL input updates form field" <|
                \_ ->
                    let
                        ( model, _ ) =
                            update (UrlInputChanged "https://test.com") mockModel
                    in
                    Expect.equal model.shortenForm.url "https://test.com"

            , test "Custom alias input updates form field" <|
                \_ ->
                    let
                        ( model, _ ) =
                            update (CustomAliasInputChanged "myalias") mockModel
                    in
                    Expect.equal model.shortenForm.customAlias "myalias"

            , test "Toggle custom alias updates form field" <|
                \_ ->
                    let
                        ( model, _ ) =
                            update (ToggleCustomAlias True) mockModel
                    in
                    Expect.equal model.shortenForm.useCustomAlias True

            , test "Expiry selection updates form field" <|
                \_ ->
                    let
                        ( model, _ ) =
                            update (ExpiresInChanged (Just 7)) mockModel
                    in
                    Expect.equal model.shortenForm.expiresIn (Just 7)
            ]

        , describe "Form Submission"
            [ test "Form resets after successful URL creation" <|
                \_ ->
                    let
                        -- Start with filled form
                        filledModel =
                            { mockModel
                                | shortenForm =
                                    { url = "https://example.com"
                                    , customAlias = "test"
                                    , useCustomAlias = True
                                    , expiresIn = Just 7
                                    }
                            }

                        -- Simulate successful API response
                        ( model, _ ) =
                            update (UrlCreated (Ok mockShortUrl)) filledModel
                    in
                    -- Form should be reset
                    Expect.all
                        [ \m -> Expect.equal m.shortenForm.url ""
                        , \m -> Expect.equal m.shortenForm.customAlias ""
                        , \m -> Expect.equal m.shortenForm.useCustomAlias False
                        , \m -> Expect.equal m.shortenForm.expiresIn Nothing
                        ]
                        model
            ]

        , describe "Form State Management"
            [ test "Form fields maintain proper state when partially filled" <|
                \_ ->
                    let
                        -- Fill URL
                        ( model1, _ ) =
                            update (UrlInputChanged "https://example.com") mockModel

                        -- Enable custom alias
                        ( model2, _ ) =
                            update (ToggleCustomAlias True) model1

                        -- Fill custom alias
                        ( model3, _ ) =
                            update (CustomAliasInputChanged "test") model2

                        -- Set expiry
                        ( finalModel, _ ) =
                            update (ExpiresInChanged (Just 7)) model3
                    in
                    -- All fields should maintain their values
                    Expect.all
                        [ \m -> Expect.equal m.shortenForm.url "https://example.com"
                        , \m -> Expect.equal m.shortenForm.customAlias "test"
                        , \m -> Expect.equal m.shortenForm.useCustomAlias True
                        , \m -> Expect.equal m.shortenForm.expiresIn (Just 7)
                        ]
                        finalModel
            ]
        ]