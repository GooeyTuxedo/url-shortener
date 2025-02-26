module View.Shortener exposing (viewShortener, viewResult)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Msg exposing (Msg(..))
import Types exposing (Model, ShortUrl, ShortenForm)


viewShortener : Model -> Element Msg
viewShortener model =
    column
        [ width fill
        , spacing 30
        ]
        [ viewShortenForm model.shortenForm model.isLoading
        , case model.currentUrl of
            Just url ->
                viewResult url

            Nothing ->
                none
        ]


viewShortenForm : ShortenForm -> Bool -> Element Msg
viewShortenForm form isLoading =
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
            (text "Create Short URL")
        , Input.text
            [ padding 12
            , Border.width 1
            , Border.rounded 6
            , Border.color (rgba 0 0 0 0.2)
            , Background.color (rgb 1 1 1)
            , width fill
            ]
            { onChange = UrlInputChanged
            , text = form.url
            , placeholder = Just (Input.placeholder [] (text "https://example.com/your/long/url"))
            , label = Input.labelAbove [ Font.bold ] (text "Long URL")
            }
        , row [ width fill, spacing 10 ]
            [ Input.checkbox
                [ paddingXY 0 10 ]
                { onChange = ToggleCustomAlias
                , icon = Input.defaultCheckbox
                , checked = form.useCustomAlias
                , label = Input.labelRight [] (text "Use custom alias")
                }
            ]
        , if form.useCustomAlias then
            Input.text
                [ padding 12
                , Border.width 1
                , Border.rounded 6
                , Border.color (rgba 0 0 0 0.2)
                , Background.color (rgb 1 1 1)
                , width fill
                ]
                { onChange = CustomAliasInputChanged
                , text = form.customAlias
                , placeholder = Just (Input.placeholder [] (text "my-custom-link"))
                , label = Input.labelAbove [ Font.bold ] (text "Custom Alias")
                }

          else
            none
        , Input.radioRow
            [ spacing 15
            , paddingXY 0 10
            ]
            { onChange = ExpiresInChanged
            , selected = Just form.expiresIn
            , label = Input.labelAbove [ Font.bold ] (text "Expires In")
            , options =
                [ Input.option Nothing (text "Never")
                , Input.option (Just 1) (text "1 day")
                , Input.option (Just 7) (text "1 week")
                , Input.option (Just 30) (text "30 days")
                , Input.option (Just 365) (text "1 year")
                ]
            }
        , Input.button
            [ padding 12
            , Border.rounded 6
            , Background.color (rgb255 52 152 219)
            , Font.color (rgb 1 1 1)
            , Font.bold
            , width fill
            , Border.shadow
                { offset = ( 0, 2 )
                , size = 0
                , blur = 4
                , color = rgba 0 0 0 0.2
                }
            , mouseOver
                [ Background.color (rgb255 41 128 185) ]
            , htmlAttribute (Html.Attributes.disabled isLoading)
            , htmlAttribute (Html.Attributes.style "cursor" (if isLoading then "wait" else "pointer"))
            ]
            { onPress = Just SubmitForm
            , label =
                if isLoading then
                    text "Creating..."

                else
                    text "Shorten URL"
            }
        ]


viewResult : ShortUrl -> Element Msg
viewResult url =
    column
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
        [ el
            [ Font.size 20
            , Font.bold
            ]
            (text "Your Short URL")
        , viewUrlCard url
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
            , Input.button
                [ padding 12
                , Border.rounded 6
                , Background.color (rgb255 52 152 219)
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
                    [ Background.color (rgb255 41 128 185) ]
                ]
                { onPress = Just (DownloadQrCode url.qrCodeUrl (url.shortCode ++ "-qrcode.png"))
                , label = text "Download QR Code"
                }
            ]
        ]


viewUrlCard : ShortUrl -> Element Msg
viewUrlCard url =
    column
        [ width fill
        , spacing 12
        , padding 16
        , Border.rounded 6
        , Border.width 1
        , Border.color (rgba 0 0 0 0.1)
        , Background.color (rgb255 250 250 250)
        ]
        [ row [ width fill, spacing 10 ]
            [ el [ Font.bold, width (px 120) ] (text "Short URL:")
            , el [ Font.color (rgb255 52 152 219), width fill ] (text url.shortUrl)
            ]
        , row [ width fill, spacing 10 ]
            [ el [ Font.bold, width (px 120) ] (text "Original URL:")
            , paragraph [ Font.color (rgb255 127 140 141), width fill ] [ text url.originalUrl ]
            ]
        , row [ width fill, spacing 10 ]
            [ el [ Font.bold, width (px 120) ] (text "Clicks:")
            , el [ width fill ] (text (String.fromInt url.clickCount))
            ]
        ]