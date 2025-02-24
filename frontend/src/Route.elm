module Route exposing (Route(..), fromUrl, toUrl)

import Types exposing (Page(..))
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | UrlDetails String
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Home (s "home")
        , Parser.map UrlDetails (s "url" </> string)
        ]


fromUrl : Url.Url -> Route
fromUrl url =
    { url | path = Maybe.withDefault "" (Just url.path), fragment = Nothing }
        |> Parser.parse routeParser
        |> Maybe.withDefault NotFound


toPage : Route -> Page
toPage route =
    case route of
        Home ->
            HomePage

        UrlDetails shortCode ->
            UrlDetailsPage shortCode

        NotFound ->
            NotFoundPage


pageFromUrl : Url.Url -> Page
pageFromUrl url =
    fromUrl url |> toPage


toUrl : Route -> String
toUrl route =
    case route of
        Home ->
            "/"

        UrlDetails shortCode ->
            "/url/" ++ shortCode

        NotFound ->
            "/not-found"