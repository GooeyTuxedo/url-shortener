module View.UrlDetails exposing (viewUrlDetails)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Msg exposing (Msg(..))
import Time
import Types exposing (Model, ShortUrl)
import View.Layout


viewUrlDetails : Model -> String -> Element Msg
viewUrlDetails model shortCode =
    View.Layout.layout model <|
        case model.currentUrl of
            Just url ->
                if url.shortCode == shortCode then
                    viewUrlDetailsContent url

                else
                    -- If the loaded URL doesn't match the requested shortCode
                    loadingOrError model shortCode

            Nothing ->
                loadingOrError model shortCode


loadingOrError : Model -> String -> Element Msg
loadingOrError model shortCode =
    column
        [ width fill
        , spacing 30
        , paddingXY 0 60
        ]
        [ if model.isLoading then
            el
                [ centerX
                , Font.size 24
                ]
                (text "Loading...")

          else
            column
                [ centerX
                , spacing 20
                ]
                [ el
                    [ Font.size 24
                    , Font.bold
                    , centerX
                    ]
                    (text "URL Not Found")
                , paragraph
                    [ Font.center ]
                    [ text ("The short URL with code '" ++ shortCode ++ "' could not be found or has expired.") ]
                , Input.button
                    [ padding 12
                    , Border.rounded 6
                    , Background.color (rgb255 52 152 219)
                    , Font.color (rgb 1 1 1)
                    , Font.bold
                    , centerX
                    , Border.shadow
                        { offset = ( 0, 2 )
                        , size = 0
                        , blur = 4
                        , color = rgba 0 0 0 0.2
                        }
                    , mouseOver
                        [ Background.color (rgb255 41 128 185) ]
                    ]
                    { onPress = Just (NavigateTo "/")
                    , label = text "Back to Home"
                    }
                ]
        ]


viewUrlDetailsContent : ShortUrl -> Element Msg
viewUrlDetailsContent url =
    column
        [ width fill
        , spacing 30
        , paddingXY 0 40
        ]
        [ el
            [ Font.size 32
            , Font.bold
            , centerX
            ]
            (text "URL Details")
        , viewUrlStats url
        , viewQrCode url
        , Input.button
            [ padding 12
            , Border.rounded 6
            , Background.color (rgb255 52 152 219)
            , Font.color (rgb 1 1 1)
            , Font.bold
            , centerX
            , width (px 200)
            , Border.shadow
                { offset = ( 0, 2 )
                , size = 0
                , blur = 4
                , color = rgba 0 0 0 0.2
                }
            , mouseOver
                [ Background.color (rgb255 41 128 185) ]
            ]
            { onPress = Just (NavigateTo "/")
            , label = text "Back to Home"
            }
        ]


viewUrlStats : ShortUrl -> Element Msg
viewUrlStats url =
    column
        [ width fill
        , spacing 20
        , padding 24
        , Background.color (rgb 1 1 1)
        , Border.rounded 10
        , Border.shadow
            { offset = ( 0, 2 )
            , size = 0
            , blur = 10
            , color = rgba 0 0 0 0.1
            }
        ]
        [ el
            [ Font.size 24
            , Font.bold
            , paddingEach { top = 0, right = 0, bottom = 10, left = 0 }
            ]
            (text "Statistics")
        , column
            [ width fill
            , spacing 16
            ]
            [ viewStatRow "Short URL" (text url.shortUrl) True
            , viewStatRow "Original URL" (paragraph [] [ text url.originalUrl ]) False
            , viewStatRow "Created" (text (formatDate url.createdAt)) False
            , viewStatRow "Expires"
                (case url.expiresAt of
                    Just date ->
                        text (formatDate date)

                    Nothing ->
                        text "Never"
                )
                False
            , viewStatRow "Clicks" (text (String.fromInt url.clickCount)) False
            ]
        , row [ width fill, spacing 20 ]
            [ Input.button
                [ padding 12
                , Border.rounded 6
                , Background.color (rgb255 46 204 113)
                , Font.color (rgb 1 1 1)
                , Font.bold
                , width (fillPortion 1)
                , Border.shadow
                    { offset = ( 0, 2 )
                    , size = 0
                    , blur = 4
                    , color = rgba 0 0 0 0.2
                    }
                , mouseOver
                    [ Background.color (rgb255 39 174 96) ]
                ]
                { onPress = Just (CopyToClipboard url.shortUrl)
                , label = text "Copy URL"
                }
            ]
        ]


viewStatRow : String -> Element Msg -> Bool -> Element Msg
viewStatRow label content isHighlighted =
    row
        [ width fill
        , padding 12
        , Border.rounded 6
        , Background.color
            (if isHighlighted then
                rgb255 240 248 255

             else
                rgb 1 1 1
            )
        , Border.width
            (if isHighlighted then
                1

             else
                0
            )
        , Border.color (rgba 0 0 0 0.1)
        ]
        [ el
            [ Font.bold
            , width (px 150)
            ]
            (text label)
        , el
            [ Font.color
                (if isHighlighted then
                    rgb255 52 152 219

                 else
                    rgb255 127 140 141
                )
            , width fill
            ]
            content
        ]


viewQrCode : ShortUrl -> Element Msg
viewQrCode url =
    column
        [ width fill
        , spacing 20
        , padding 24
        , Background.color (rgb 1 1 1)
        , Border.rounded 10
        , Border.shadow
            { offset = ( 0, 2 )
            , size = 0
            , blur = 10
            , color = rgba 0 0 0 0.1
            }
        ]
        [ el
            [ Font.size 24
            , Font.bold
            , paddingEach { top = 0, right = 0, bottom = 10, left = 0 }
            ]
            (text "QR Code")
        , el
            [ centerX
            , padding 20
            ]
            (image
                [ width (px 200)
                , height (px 200)
                , centerX
                ]
                { src = url.qrCodeUrl
                , description = "QR Code for " ++ url.shortUrl
                }
            )
        , Input.button
            [ padding 12
            , Border.rounded 6
            , Background.color (rgb255 52 152 219)
            , Font.color (rgb 1 1 1)
            , Font.bold
            , centerX
            , width (px 250)
            , Border.shadow
                { offset = ( 0, 2 )
                , size = 0
                , blur = 4
                , color = rgba 0 0 0 0.2
                }
            , mouseOver
                [ Background.color (rgb255 41 128 185) ]
            ]
            { onPress = Just (DownloadQrCode url.qrCodeUrl (url.shortCode ++ "-qrcode.png"))
            , label = text "Download QR Code"
            }
        ]


formatDate : Time.Posix -> String
formatDate time =
    -- This is a placeholder. In a real app, use a proper date formatting library
    "2025-01-01"



-- Replace with actual date formatting logic
