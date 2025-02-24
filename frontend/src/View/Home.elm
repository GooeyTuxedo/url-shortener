module View.Home exposing (viewHome)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes
import Msg exposing (Msg(..))
import Types exposing (Model, ShortUrl, ShortenForm)
import View.Layout exposing (layout)
import View.Shortener exposing (viewShortener)
import View.UrlList exposing (viewUrlList)


viewHome : Model -> Element Msg
viewHome model =
    layout model <|
        column
            [ width fill
            , spacing 40
            , paddingXY 0 40
            ]
            [ viewHero
            , viewShortener model
            , if List.isEmpty model.urls then
                viewEmptyState

              else
                viewUrlList model.urls
            ]


viewHero : Element Msg
viewHero =
    column
        [ width fill
        , spacing 20
        , paddingXY 0 40
        , Font.center
        ]
        [ el
            [ Font.size 48
            , Font.bold
            , centerX
            ]
            (text "Shorten Your Links")
        , paragraph
            [ Font.size 20
            , Font.color (rgba 0 0 0 0.7)
            , centerX
            , width (fill |> maximum 700)
            ]
            [ text "Create short, memorable links that redirect to your long URLs. Track visits and easily share your content." ]
        ]


viewEmptyState : Element Msg
viewEmptyState =
    column
        [ width fill
        , spacing 20
        , paddingXY 0 40
        , Font.center
        ]
        [ el
            [ Font.size 24
            , Font.bold
            , centerX
            ]
            (text "No URLs yet")
        , paragraph
            [ Font.size 16
            , Font.color (rgba 0 0 0 0.7)
            , centerX
            , width (fill |> maximum 500)
            ]
            [ text "Create your first short URL using the form above. Your shortened URLs will appear here." ]
        ]
