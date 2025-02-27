module TypesTests exposing (suite)

import Expect
import Test exposing (..)
import Time
import Types exposing (..)


suite : Test
suite =
    describe "Types Module"
        [ describe "ShortUrl"
            [ test "ShortUrl record has expected fields" <|
                \_ ->
                    let
                        url =
                            { shortUrl = "http://short.url/abc123"
                            , originalUrl = "http://example.com/long/url"
                            , shortCode = "abc123"
                            , createdAt = Time.millisToPosix 0
                            , expiresAt = Just (Time.millisToPosix 86400000) -- 1 day in milliseconds
                            , clickCount = 0
                            , qrCodeUrl = "http://api.example.com/qr/abc123"
                            }
                    in
                    Expect.all
                        [ \u -> Expect.equal u.shortUrl "http://short.url/abc123"
                        , \u -> Expect.equal u.originalUrl "http://example.com/long/url"
                        , \u -> Expect.equal u.shortCode "abc123"
                        , \u -> Expect.equal u.createdAt (Time.millisToPosix 0)
                        , \u -> Expect.equal u.expiresAt (Just (Time.millisToPosix 86400000))
                        , \u -> Expect.equal u.clickCount 0
                        , \u -> Expect.equal u.qrCodeUrl "http://api.example.com/qr/abc123"
                        ]
                        url
            ]
        , describe "CreateShortUrlRequest"
            [ test "CreateShortUrlRequest record has expected fields" <|
                \_ ->
                    let
                        request =
                            { longUrl = "http://example.com/long/url"
                            , customAlias = Just "custom"
                            , expiresIn = Just 7
                            }
                    in
                    Expect.all
                        [ \r -> Expect.equal r.longUrl "http://example.com/long/url"
                        , \r -> Expect.equal r.customAlias (Just "custom")
                        , \r -> Expect.equal r.expiresIn (Just 7)
                        ]
                        request
            ]
        , describe "NotificationType"
            [ test "NotificationType has expected variants" <|
                \_ ->
                    let
                        -- Test by creating all variants to ensure they exist
                        types =
                            [ Success, Error, Info, Warning ]
                    in
                    Expect.equal (List.length types) 4
            ]
        ]