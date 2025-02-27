module ApiTests exposing (suite)

import Api
import Expect
import Json.Encode as Encode
import Test exposing (..)
import Types exposing (CreateShortUrlRequest, ShortUrl)
import Url.Builder

suite : Test
suite =
    describe "Api Module"
        [ test "createShortUrlRequest encodes longUrl correctly" <|
            \_ ->
                let
                    request =
                        { longUrl = "https://example.com/some/long/url"
                        , customAlias = Nothing
                        , expiresIn = Nothing
                        }

                    expected =
                        Encode.object
                            [ ( "longUrl", Encode.string "https://example.com/some/long/url" )
                            , ( "customAlias", Encode.null )
                            , ( "expiresIn", Encode.null )
                            ]
                in
                -- This is a test of the encoder function which we need to expose for testing
                Expect.equal (Api.encodeCreateShortUrlRequest request) expected
        , test "createShortUrlRequest encodes customAlias correctly" <|
            \_ ->
                let
                    request =
                        { longUrl = "https://example.com/some/long/url"
                        , customAlias = Just "my-custom-alias"
                        , expiresIn = Nothing
                        }

                    expected =
                        Encode.object
                            [ ( "longUrl", Encode.string "https://example.com/some/long/url" )
                            , ( "customAlias", Encode.string "my-custom-alias" )
                            , ( "expiresIn", Encode.null )
                            ]
                in
                Expect.equal (Api.encodeCreateShortUrlRequest request) expected
        , test "createShortUrlRequest encodes expiresIn correctly" <|
            \_ ->
                let
                    request =
                        { longUrl = "https://example.com/some/long/url"
                        , customAlias = Nothing
                        , expiresIn = Just 7
                        }

                    expected =
                        Encode.object
                            [ ( "longUrl", Encode.string "https://example.com/some/long/url" )
                            , ( "customAlias", Encode.null )
                            , ( "expiresIn", Encode.int 7 )
                            ]
                in
                Expect.equal (Api.encodeCreateShortUrlRequest request) expected
        , test "apiUrl constructs correct URL with no query params" <|
            \_ ->
                let
                    baseUrl =
                        "http://localhost:8080"

                    path =
                        "shorten"

                    queryParams =
                        []

                    expected =
                        "http://localhost:8080/api/shorten"
                in
                Expect.equal (Api.apiUrl baseUrl path queryParams) expected
        , test "apiUrl constructs correct URL with query params" <|
            \_ ->
                let
                    baseUrl =
                        "http://localhost:8080"

                    path =
                        "urls"

                    queryParams =
                        [ Url.Builder.string "shortCode" "abc123" ]

                    expected =
                        "http://localhost:8080/api/urls?shortCode=abc123"
                in
                Expect.equal (Api.apiUrl baseUrl path queryParams) expected
        ]

-- Note: To make these tests work, you'll need to expose some 
-- of the internal functions in Api.elm by updating the module definition to:
-- module Api exposing (createShortUrl, getShortUrl, getShortUrls, encodeCreateShortUrlRequest, apiUrl)