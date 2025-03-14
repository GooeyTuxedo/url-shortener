module View.ClientDashboard exposing (viewClientDashboard)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Msg exposing (Msg(..))
import Types exposing (Model)
import View.Layout
import View.UrlList exposing (viewUrlList)


viewClientDashboard : Model -> Element Msg
viewClientDashboard model =
    View.Layout.layout model <|
        column
            [ width fill
            , spacing 40
            , paddingXY 0 40
            ]
            [ viewDashboardHeader
            , viewClientIdManager model
            , viewClientActionButtons model
            , if List.isEmpty model.urls then
                viewEmptyState
              else
                viewUrlList model.urls
            ]


viewDashboardHeader : Element Msg
viewDashboardHeader =
    column
        [ width fill
        , spacing 20
        , Font.center
        ]
        [ el
            [ Font.size 40
            , Font.bold
            , centerX
            ]
            (text "Your URL Dashboard")
        , paragraph
            [ Font.size 18
            , Font.color (rgba 0 0 0 0.7)
            , centerX
            , width (fill |> maximum 600)
            ]
            [ text "Manage all your shortened URLs in one place. View stats, create new links, and more." ]
        ]


viewClientIdManager : Model -> Element Msg
viewClientIdManager model =
    column
        [ width (fill |> maximum 600)
        , spacing 15
        , padding 20
        , centerX
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
            [ Font.size 20
            , Font.bold
            ]
            (text "Your Client ID")
        , paragraph
            [ Font.color (rgba 0 0 0 0.6)
            , paddingEach { top = 0, right = 0, bottom = 15, left = 0 }
            ]
            [ text "This ID uniquely identifies you and is used to track your shortened URLs." ]
        , row
            [ width fill
            , spacing 15
            ]
            [ Input.text
                [ Border.width 1
                , Border.rounded 6
                , Border.color (rgba 0 0 0 0.2)
                , padding 12
                , width fill
                ]
                { onChange = ClientIdChanged
                , text = model.tempClientId
                , placeholder =
                    Just
                        (Input.placeholder []
                            (text
                                (case model.clientId of
                                    Just id ->
                                        id

                                    Nothing ->
                                        "Enter a client ID"
                                )
                            )
                        )
                , label = Input.labelHidden "Client ID"
                }
            , Input.button
                [ padding 12
                , Background.color (rgb255 52 152 219)
                , Font.color (rgb 1 1 1)
                , Font.bold
                , Border.rounded 6
                , width (px 100)
                , Border.shadow
                    { offset = ( 0, 2 )
                    , size = 0
                    , blur = 4
                    , color = rgba 0 0 0 0.2
                    }
                , mouseOver
                    [ Background.color (rgb255 41 128 185) ]
                ]
                { onPress = Just SaveClientId
                , label = el [ centerX ] (text "Save")
                }
            ]
        ]


viewClientActionButtons : Model -> Element Msg
viewClientActionButtons model =
    row
        [ width (fill |> maximum 600)
        , spacing 20
        , centerX
        ]
        [ Input.button
            [ padding 15
            , Background.color (rgb255 46 204 113)
            , Font.color (rgb 1 1 1)
            , Font.bold
            , width (fillPortion 1)
            , Border.rounded 6
            , Border.shadow
                { offset = ( 0, 2 )
                , size = 0
                , blur = 4
                , color = rgba 0 0 0 0.2
                }
            , mouseOver
                [ Background.color (rgb255 39 174 96) ]
            ]
            { onPress = Just (NavigateTo "/")
            , label = el [ centerX ] (text "Create New URL")
            }
        , Input.button
            [ padding 15
            , Background.color (rgb255 52 152 219)
            , Font.color (rgb 1 1 1)
            , Font.bold
            , width (fillPortion 1)
            , Border.rounded 6
            , Border.shadow
                { offset = ( 0, 2 )
                , size = 0
                , blur = 4
                , color = rgba 0 0 0 0.2
                }
            , mouseOver
                [ Background.color (rgb255 41 128 185) ]
            ]
            { onPress = 
                case model.clientId of
                    Just _ -> Just LoadClientUrls
                    Nothing -> Nothing
            , label = el [ centerX ] (text "Refresh URLs")
            }
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
            (text "No URLs Found")
        , paragraph
            [ Font.size 16
            , Font.color (rgba 0 0 0 0.7)
            , centerX
            , width (fill |> maximum 500)
            ]
            [ text "You haven't created any URLs with your current client ID yet. Go to the home page to create your first short URL." ]
        ]