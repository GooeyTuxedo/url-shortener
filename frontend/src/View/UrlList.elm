module View.UrlList exposing (viewUrlList)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Msg exposing (Msg(..))
import Types exposing (ShortUrl)


viewUrlList : List ShortUrl -> Element Msg
viewUrlList urls =
    column
        [ width fill
        , spacing 20
        ]
        [ el
            [ Font.size 24
            , Font.bold
            , paddingEach { top = 0, right = 0, bottom = 10, left = 0 }
            ]
            (text "Your URLs")
        , column
            [ width fill
            , spacing 16
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
            (List.map viewUrlItem urls)
        ]


viewUrlItem : ShortUrl -> Element Msg
viewUrlItem url =
    row
        [ width fill
        , spacing 10
        , padding 16
        , Border.rounded 6
        , Border.width 1
        , Border.color (rgba 0 0 0 0.1)
        , Background.color (rgb255 250 250 250)
        , pointer
        , mouseOver
            [ Background.color (rgb255 240 240 240) ]
        , onClick (NavigateTo ("/url/" ++ url.shortCode))
        ]
        [ column [ width (fillPortion 5), spacing 8 ]
            [ el [ Font.bold ] (text url.shortCode)
            , paragraph
                [ Font.color (rgb255 127 140 141)
                , Font.size 14
                , clipX
                ]
                [ text url.originalUrl ]
            ]
        , column [ width (fillPortion 1), alignRight, Font.alignRight, spacing 8 ]
            [ el [ Font.bold ] (text (String.fromInt url.clickCount))
            , el [ Font.size 14, Font.color (rgb255 127 140 141) ] (text "clicks")
            ]
        ]