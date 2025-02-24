module View.NotFound exposing (viewNotFound)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Msg exposing (Msg(..))
import Types exposing (Model)
import View.Layout exposing (layout)


viewNotFound : Model -> Element Msg
viewNotFound model =
    layout model <|
        column
            [ width fill
            , spacing 30
            , paddingXY 0 60
            , Font.center
            ]
            [ el
                [ Font.size 72
                , Font.bold
                , centerX
                , Font.color (rgb255 52 152 219)
                ]
                (text "404")
            , el
                [ Font.size 36
                , Font.bold
                , centerX
                , paddingXY 0 20
                ]
                (text "Page Not Found")
            , paragraph
                [ Font.size 18
                , Font.color (rgba 0 0 0 0.7)
                , centerX
                , width (fill |> maximum 600)
                ]
                [ text "The page you're looking for doesn't exist or has been moved." ]
            , Input.button
                [ padding 12
                , Border.rounded 6
                , Background.color (rgb255 52 152 219)
                , Font.color (rgb 1 1 1)
                , Font.bold
                , width (px 200)
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
