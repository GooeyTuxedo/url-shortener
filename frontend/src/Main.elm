module Main exposing (main, update)

import Api
import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (Html)
import Http
import Msg exposing (Msg(..))
import Ports exposing (clipboardStatus, qrCodeDownloaded, windowResize)
import Process
import Route
import Task
import TestableNavigation
import Time
import Types exposing (..)
import Url
import View.Home
import View.NotFound
import View.UrlDetails


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        page =
            Route.fromUrl url |> toPage

        model =
            { apiUrl = flags.apiUrl
            , windowWidth = flags.windowWidth
            , page = page
            , urls = []
            , currentUrl = Nothing
            , shortenForm =
                { url = ""
                , customAlias = ""
                , useCustomAlias = False
                , expiresIn = Nothing
                }
            , notification = Nothing
            , isLoading = False
            , errorMessage = Nothing
            , navKey = TestableNavigation.fromReal key
            }
    in
    ( model
    , Cmd.batch
        [ Task.perform (\time -> TimeReceived time) Time.now
        , loadPage model page
        ]
    )


toPage : Route.Route -> Page
toPage route =
    case route of
        Route.Home ->
            HomePage

        Route.UrlDetails shortCode ->
            UrlDetailsPage shortCode

        Route.NotFound ->
            NotFoundPage


loadPage : Model -> Page -> Cmd Msg
loadPage model page =
    case page of
        HomePage ->
            -- Could load recent URLs here if you have an API endpoint for that
            Cmd.none

        UrlDetailsPage shortCode ->
            Api.getShortUrl model.apiUrl shortCode UrlDetailsLoaded

        NotFoundPage ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChanged url ->
            let
                page =
                    Route.fromUrl url |> toPage
            in
            ( { model | page = page, isLoading = True }
            , loadPage model page
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , TestableNavigation.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , TestableNavigation.load href
                    )

        WindowResized width ->
            ( { model | windowWidth = width }, Cmd.none )

        UrlInputChanged url ->
            let
                form =
                    model.shortenForm

                newForm =
                    { form | url = url }
            in
            ( { model | shortenForm = newForm }, Cmd.none )

        CustomAliasInputChanged alias ->
            let
                form =
                    model.shortenForm

                newForm =
                    { form | customAlias = alias }
            in
            ( { model | shortenForm = newForm }, Cmd.none )

        ToggleCustomAlias useCustom ->
            let
                form =
                    model.shortenForm

                newForm =
                    { form | useCustomAlias = useCustom }
            in
            ( { model | shortenForm = newForm }, Cmd.none )

        ExpiresInChanged days ->
            let
                form =
                    model.shortenForm

                newForm =
                    { form | expiresIn = days }
            in
            ( { model | shortenForm = newForm }, Cmd.none )

        SubmitForm ->
            if String.isEmpty model.shortenForm.url then
                ( { model | notification = Just { message = "Please enter a URL", notificationType = Error } }
                , Cmd.batch [ Task.perform (\_ -> DismissNotification) (Process.sleep 3000) ]
                )

            else
                let
                    request =
                        { longUrl = model.shortenForm.url
                        , customAlias =
                            if model.shortenForm.useCustomAlias && not (String.isEmpty model.shortenForm.customAlias) then
                                Just model.shortenForm.customAlias

                            else
                                Nothing
                        , expiresIn = model.shortenForm.expiresIn
                        }
                in
                ( { model | isLoading = True }
                , Api.createShortUrl model.apiUrl request UrlCreated
                )

        UrlCreated result ->
            case result of
                Ok url ->
                    let
                        updatedUrls =
                            url :: model.urls

                        newForm =
                            { url = ""
                            , customAlias = ""
                            , useCustomAlias = False
                            , expiresIn = Nothing
                            }
                    in
                    ( { model
                        | urls = updatedUrls
                        , currentUrl = Just url
                        , shortenForm = newForm
                        , isLoading = False
                        , notification = Just { message = "URL successfully shortened!", notificationType = Success }
                      }
                    , Task.perform (\_ -> DismissNotification) (Process.sleep 3000)
                    )

                Err error ->
                    let
                        errorMsg =
                            case error of
                                Http.BadStatus 400 ->
                                    "Invalid URL or parameters"

                                Http.BadStatus 409 ->
                                    "Custom alias already in use"

                                Http.BadStatus 429 ->
                                    "Rate limit exceeded, please try again later"

                                _ ->
                                    "Failed to create short URL. Please try again."
                    in
                    ( { model
                        | isLoading = False
                        , notification = Just { message = errorMsg, notificationType = Error }
                      }
                    , Task.perform (\_ -> DismissNotification) (Process.sleep 3000)
                    )

        LoadUrls ->
            ( { model | isLoading = True }
            , Api.getShortUrls model.apiUrl UrlsLoaded
            )

        UrlsLoaded result ->
            case result of
                Ok urls ->
                    ( { model | urls = urls, isLoading = False }, Cmd.none )

                Err _ ->
                    ( { model
                        | isLoading = False
                        , notification = Just { message = "Failed to load URLs", notificationType = Error }
                      }
                    , Task.perform (\_ -> DismissNotification) (Process.sleep 3000)
                    )

        LoadUrlDetails shortCode ->
            ( { model | isLoading = True }
            , Api.getShortUrl model.apiUrl shortCode UrlDetailsLoaded
            )

        UrlDetailsLoaded result ->
            case result of
                Ok url ->
                    ( { model | currentUrl = Just url, isLoading = False }, Cmd.none )

                Err _ ->
                    ( { model
                        | isLoading = False
                        , notification = Just { message = "Failed to load URL details", notificationType = Error }
                      }
                    , Task.perform (\_ -> DismissNotification) (Process.sleep 3000)
                    )

        NavigateTo path ->
            ( model
            , TestableNavigation.pushUrl model.navKey path
            )

        ShowNotification message notificationType ->
            ( { model | notification = Just { message = message, notificationType = notificationType } }
            , Task.perform (\_ -> DismissNotification) (Process.sleep 3000)
            )

        DismissNotification ->
            ( { model | notification = Nothing }, Cmd.none )

        CopyToClipboard text ->
            ( model
            , Ports.copyToClipboard text
            )

        ClipboardResult status ->
            if status.success then
                ( { model
                    | notification = Just { message = "Copied to clipboard!", notificationType = Success }
                  }
                , Task.perform (\_ -> DismissNotification) (Process.sleep 3000)
                )

            else
                ( { model
                    | notification = Just { message = "Failed to copy to clipboard", notificationType = Error }
                  }
                , Task.perform (\_ -> DismissNotification) (Process.sleep 3000)
                )

        DownloadQrCode url filename ->
            ( model
            , Ports.downloadQrCode { url = url, filename = filename }
            )

        QrCodeDownloaded status ->
            if status.success then
                ( { model
                    | notification = Just { message = "QR code downloaded!", notificationType = Success }
                  }
                , Task.perform (\_ -> DismissNotification) (Process.sleep 3000)
                )

            else
                ( { model
                    | notification = Just { message = "Failed to download QR code", notificationType = Error }
                  }
                , Task.perform (\_ -> DismissNotification) (Process.sleep 3000)
                )

        TimeReceived time ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = pageTitle model.page
    , body = [ viewBody model ]
    }


pageTitle : Page -> String
pageTitle page =
    case page of
        HomePage ->
            "URL Shortener"

        UrlDetailsPage shortCode ->
            "URL Details - " ++ shortCode

        NotFoundPage ->
            "Page Not Found"


viewBody : Model -> Html Msg
viewBody model =
    Element.layout [] <|
        case model.page of
            HomePage ->
                View.Home.viewHome model

            UrlDetailsPage shortCode ->
                View.UrlDetails.viewUrlDetails model shortCode

            NotFoundPage ->
                View.NotFound.viewNotFound model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ windowResize WindowResized
        , clipboardStatus ClipboardResult
        , qrCodeDownloaded QrCodeDownloaded
        ]
