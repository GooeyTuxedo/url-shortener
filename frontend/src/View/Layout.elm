module View.Layout exposing (layout, viewFooter, viewHeader, viewNotification)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Region as Region
import Html.Attributes
import Msg exposing (Msg(..))
import Route exposing (Route(..))
import Types exposing (Model, Notification, NotificationType(..))


colors =
    { primary = rgb255 52 152 219 -- #3498db
    , secondary = rgb255 41 128 185 -- #2980b9
    , accent = rgb255 231 76 60 -- #e74c3c
    , success = rgb255 46 204 113 -- #2ecc71
    , warning = rgb255 243 156 18 -- #f39c12
    , error = rgb255 231 76 60 -- #e74c3c
    , bgColor = rgb255 245 247 250 -- #f5f7fa
    , bgCard = rgb255 255 255 255 -- #ffffff
    , textPrimary = rgb255 44 62 80 -- #2c3e50
    , textSecondary = rgb255 127 140 141 -- #7f8c8d
    , borderColor = rgb255 223 230 233 -- #dfe6e9
    }


layout : Model -> Element Msg -> Element Msg
layout model content =
    column
        [ width fill
        , height fill
        , Background.color colors.bgColor
        , Font.family
            [ Font.typeface "Inter"
            , Font.sansSerif
            ]
        , Font.color colors.textPrimary
        , paddingXY 0 0
        , inFront (viewNotificationContainer model.notification)
        ]
        [ viewHeader model
        , el
            [ width (fill |> maximum 1200)
            , height fill
            , centerX
            , paddingXY 20 0
            ]
            content
        , viewFooter
        ]


viewHeader : Model -> Element Msg
viewHeader model =
    row
        [ width fill
        , paddingXY 20 15
        , Background.color colors.primary
        , Font.color (rgb 1 1 1)
        , Border.shadow
            { offset = ( 0, 2 )
            , size = 1
            , blur = 4
            , color = rgba 0 0 0 0.2
            }
        , Region.navigation
        ]
        [ row
            [ width (fill |> maximum 1200)
            , centerX
            , spacing 20
            ]
            [ el
                [ Font.bold
                , Font.size 24
                , pointer
                , onClick (NavigateTo "/")
                ]
                (text "URL Shortener")
            , el [ width fill ] none
            , if model.windowWidth > 768 then
                row [ spacing 15 ]
                    [ navLink "Home" "/"
                    ]

              else
                none
            ]
        ]


navLink : String -> String -> Element Msg
navLink label path =
    el
        [ pointer
        , paddingXY 10 8
        , Border.rounded 4
        , mouseOver [ Background.color colors.secondary ]
        , onClick (NavigateTo path)
        ]
        (text label)


viewFooter : Element Msg
viewFooter =
    row
        [ width fill
        , paddingXY 20 15
        , Background.color (rgba 0 0 0 0.05)
        , Font.color colors.textSecondary
        , Font.size 14
        , Border.shadow
            { offset = ( 0, -2 )
            , size = 1
            , blur = 4
            , color = rgba 0 0 0 0.1
            }
        , Region.footer
        ]
        [ row
            [ width (fill |> maximum 1200)
            , centerX
            , spacing 20
            ]
            [ text "© 2025 URL Shortener"
            , el [ width fill ] none
            , row [ spacing 15 ]
                [ link [] { url = "https://github.com/GooeyTuxedo/url-shortener", label = text "GitHub" }
                ]
            ]
        ]


viewNotificationContainer : Maybe Notification -> Element Msg
viewNotificationContainer maybeNotification =
    case maybeNotification of
        Just notification ->
            el
                [ width fill
                , paddingXY 20 10
                , alignTop
                ]
                (el
                    [ centerX
                    , width (fill |> maximum 600)
                    ]
                    (viewNotification notification)
                )

        Nothing ->
            none


viewNotification : Notification -> Element Msg
viewNotification notification =
    let
        ( bgColor, textColor ) =
            case notification.notificationType of
                Success ->
                    ( colors.success, rgb 1 1 1 )

                Error ->
                    ( colors.error, rgb 1 1 1 )

                Warning ->
                    ( colors.warning, rgb 0 0 0 )

                Info ->
                    ( colors.primary, rgb 1 1 1 )
    in
    row
        [ width fill
        , paddingXY 16 12
        , Border.rounded 6
        , Background.color bgColor
        , Font.color textColor
        , Border.shadow
            { offset = ( 0, 2 )
            , size = 1
            , blur = 4
            , color = rgba 0 0 0 0.2
            }
        , class "notification"
        ]
        [ paragraph [] [ text notification.message ]
        , el [ alignRight, pointer, onClick DismissNotification ] (text "×")
        ]


class : String -> Attribute msg
class className =
    htmlAttribute (Html.Attributes.class className)
