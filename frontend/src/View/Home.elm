module View.Home exposing (viewHome)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Msg exposing (Msg(..))
import Types exposing (Model)
import View.Layout
import View.Shortener exposing (viewShortener)
import View.UrlList exposing (viewUrlList)


viewHome : Model -> Element Msg
viewHome model =
    View.Layout.layout model <|
        column
            [ width fill
            , spacing 40
            , paddingXY 0 40
            ]
            [ viewHero
            , viewClientIdInfo model
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
        , paddingXY 0 20
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


viewClientIdInfo : Model -> Element Msg
viewClientIdInfo model =
    case model.clientId of
        Just clientId ->
            el
                [ width (fill |> maximum 700)
                , paddingXY 20 15
                , centerX
                , Background.color (rgba 0 0 0 0.05)
                , Border.rounded 8
                , Font.center
                ]
                (paragraph []
                    [ text "You are creating URLs as "
                    , el [ Font.bold ] (text clientId)
                    , text ". All URLs you create will be linked to this client ID. "
                    , el
                        [ Font.color (rgb255 41 128 185)
                        , pointer
                        , mouseOver [ Font.color (rgb255 52 152 219) ]
                        , onClick (NavigateTo "/my-urls")
                        ]
                        (text "Manage your URLs")
                    , text "."
                    ]
                )
        
        Nothing ->
            el
                [ width (fill |> maximum 700)
                , paddingXY 20 15
                , centerX
                , Background.color (rgba 243 156 18 0.2)
                , Border.rounded 8
                , Font.center
                ]
                (paragraph []
                    [ text "You haven't set a client ID yet. "
                    , el
                        [ Font.color (rgb255 41 128 185)
                        , pointer
                        , mouseOver [ Font.color (rgb255 52 152 219) ]
                        , onClick (NavigateTo "/my-urls")
                        ]
                        (text "Set one now")
                    , text " to keep track of your shortened URLs."
                    ]
                )


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
