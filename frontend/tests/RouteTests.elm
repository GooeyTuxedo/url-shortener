module RouteTests exposing (suite)

import Expect
import Route exposing (Route(..))
import Test exposing (..)
import Url


suite : Test
suite =
    describe "Route Module"
        [ describe "fromUrl"
            [ test "parses root URL to Home route" <|
                \_ ->
                    let
                        url =
                            { protocol = Url.Http
                            , host = "localhost"
                            , port_ = Just 1234
                            , path = "/"
                            , query = Nothing
                            , fragment = Nothing
                            }
                    in
                    Expect.equal (Route.fromUrl url) Route.Home
            , test "parses /home URL to Home route" <|
                \_ ->
                    let
                        url =
                            { protocol = Url.Http
                            , host = "localhost"
                            , port_ = Just 1234
                            , path = "/home"
                            , query = Nothing
                            , fragment = Nothing
                            }
                    in
                    Expect.equal (Route.fromUrl url) Route.Home
            , test "parses /url/{shortCode} URL to UrlDetails route" <|
                \_ ->
                    let
                        url =
                            { protocol = Url.Http
                            , host = "localhost"
                            , port_ = Just 1234
                            , path = "/url/abc123"
                            , query = Nothing
                            , fragment = Nothing
                            }
                    in
                    Expect.equal (Route.fromUrl url) (Route.UrlDetails "abc123")
            , test "parses unknown URL to NotFound route" <|
                \_ ->
                    let
                        url =
                            { protocol = Url.Http
                            , host = "localhost"
                            , port_ = Just 1234
                            , path = "/unknown/path"
                            , query = Nothing
                            , fragment = Nothing
                            }
                    in
                    Expect.equal (Route.fromUrl url) Route.NotFound
            ]
        , describe "toUrl"
            [ test "generates correct URL for Home route" <|
                \_ ->
                    Expect.equal (Route.toUrl Route.Home) "/"
            , test "generates correct URL for UrlDetails route" <|
                \_ ->
                    Expect.equal (Route.toUrl (Route.UrlDetails "abc123")) "/url/abc123"
            , test "generates correct URL for NotFound route" <|
                \_ ->
                    Expect.equal (Route.toUrl Route.NotFound) "/not-found"
            ]
        ]