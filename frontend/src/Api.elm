module Api exposing (createShortUrl, getShortUrl, getShortUrls)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time
import Types exposing (CreateShortUrlRequest, ShortUrl)
import Url.Builder as UrlBuilder


-- API URLs


apiUrl : String -> String -> List UrlBuilder.QueryParameter -> String
apiUrl baseUrl path queryParams =
    UrlBuilder.crossOrigin baseUrl ("api" :: path :: []) queryParams


-- JSON Decoders


shortUrlDecoder : Decoder ShortUrl
shortUrlDecoder =
    Decode.succeed ShortUrl
        |> required "shortUrl" Decode.string
        |> required "originalUrl" Decode.string
        |> required "shortCode" Decode.string
        |> required "createdAt" iso8601Decoder
        |> optional "expiresAt" (Decode.nullable iso8601Decoder) Nothing
        |> required "clickCount" Decode.int
        |> required "qrCodeUrl" Decode.string


iso8601Decoder : Decoder Time.Posix
iso8601Decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case iso8601StringToPosix str of
                    Ok posix ->
                        Decode.succeed posix

                    Err error ->
                        Decode.fail error
            )


iso8601StringToPosix : String -> Result String Time.Posix
iso8601StringToPosix str =
    -- This is a simplified implementation
    -- In a real app, use rtfeldman/elm-iso8601-date-strings
    Ok (Time.millisToPosix 0)


-- JSON Encoders


encodeCreateShortUrlRequest : CreateShortUrlRequest -> Encode.Value
encodeCreateShortUrlRequest request =
    Encode.object
        [ ( "longUrl", Encode.string request.longUrl )
        , ( "customAlias"
          , case request.customAlias of
                Just alias ->
                    Encode.string alias

                Nothing ->
                    Encode.null
          )
        , ( "expiresIn"
          , case request.expiresIn of
                Just days ->
                    Encode.int days

                Nothing ->
                    Encode.null
          )
        ]


-- API Requests


createShortUrl : String -> CreateShortUrlRequest -> (Result Http.Error ShortUrl -> msg) -> Cmd msg
createShortUrl baseApiUrl request toMsg =
    Http.post
        { url = apiUrl baseApiUrl "shorten" []
        , body = Http.jsonBody (encodeCreateShortUrlRequest request)
        , expect = Http.expectJson toMsg shortUrlDecoder
        }


getShortUrls : String -> (Result Http.Error (List ShortUrl) -> msg) -> Cmd msg
getShortUrls baseApiUrl toMsg =
    -- Note: This endpoint doesn't exist in the current backend
    -- You'll need to add it or modify this to work with your actual API
    Http.get
        { url = apiUrl baseApiUrl "urls" []
        , expect = Http.expectJson toMsg (Decode.list shortUrlDecoder)
        }


getShortUrl : String -> String -> (Result Http.Error ShortUrl -> msg) -> Cmd msg
getShortUrl baseApiUrl shortCode toMsg =
    Http.get
        { url = apiUrl baseApiUrl "urls" [ UrlBuilder.string "shortCode" shortCode ]
        , expect = Http.expectJson toMsg shortUrlDecoder
        }