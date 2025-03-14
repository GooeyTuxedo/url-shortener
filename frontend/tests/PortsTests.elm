module PortsTests exposing (suite)

import Expect
import Main.Internal exposing (update)
import Msg exposing (Msg(..))
import Test exposing (..)
import TestHelpers exposing (mockModel)
import Types exposing (..)


{-
   Note: Testing ports fully requires JavaScript interop that's beyond
   what elm-test can do directly. These tests verify command creation
   and response handling but not the actual JS execution.
-}


suite : Test
suite =
    describe "Ports"
        [ describe "Copy to Clipboard"
            [ test "CopyToClipboard msg generates a command" <|
                \_ ->
                    let
                        ( _, cmd ) =
                            update (CopyToClipboard "http://short.url/abc123") mockModel
                    in
                    -- We can't inspect the Cmd itself, but we can verify it's not Cmd.none
                    Expect.notEqual cmd Cmd.none

            , test "ClipboardResult success shows notification" <|
                \_ ->
                    let
                        successResult =
                            { success = True, text = "http://short.url/abc123" }

                        ( model, _ ) =
                            update (ClipboardResult successResult) mockModel
                    in
                    -- Should show success notification
                    case model.notification of
                        Just notification ->
                            Expect.equal notification.notificationType Success

                        Nothing ->
                            Expect.fail "Expected success notification"

            , test "ClipboardResult failure shows error notification" <|
                \_ ->
                    let
                        failureResult =
                            { success = False, text = "http://short.url/abc123" }

                        ( model, _ ) =
                            update (ClipboardResult failureResult) mockModel
                    in
                    -- Should show error notification
                    case model.notification of
                        Just notification ->
                            Expect.equal notification.notificationType Error

                        Nothing ->
                            Expect.fail "Expected error notification"
            ]

        , describe "QR Code Download"
            [ test "DownloadQrCode msg generates a command" <|
                \_ ->
                    let
                        ( _, cmd ) =
                            update (DownloadQrCode "http://api.example.com/qr/abc123" "qrcode.png") mockModel
                    in
                    -- We can't inspect the Cmd itself, but we can verify it's not Cmd.none
                    Expect.notEqual cmd Cmd.none

            , test "QrCodeDownloaded success shows notification" <|
                \_ ->
                    let
                        successResult =
                            { success = True }

                        ( model, _ ) =
                            update (QrCodeDownloaded successResult) mockModel
                    in
                    -- Should show success notification
                    case model.notification of
                        Just notification ->
                            Expect.equal notification.notificationType Success

                        Nothing ->
                            Expect.fail "Expected success notification"

            , test "QrCodeDownloaded failure shows error notification" <|
                \_ ->
                    let
                        failureResult =
                            { success = False }

                        ( model, _ ) =
                            update (QrCodeDownloaded failureResult) mockModel
                    in
                    -- Should show error notification
                    case model.notification of
                        Just notification ->
                            Expect.equal notification.notificationType Error

                        Nothing ->
                            Expect.fail "Expected error notification"
            ]

        , describe "Window Resize"
            [ test "WindowResized updates the model" <|
                \_ ->
                    let
                        ( model, _ ) =
                            update (WindowResized 768) mockModel
                    in
                    Expect.equal model.windowWidth 768
            ]
            
        , describe "Client ID Storage"
            [ test "ClientIdChanged updates tempClientId" <|
                \_ ->
                    let
                        ( model, _ ) =
                            update (ClientIdChanged "test-client") mockModel
                    in
                    Expect.equal model.tempClientId "test-client"
                    
            , test "SaveClientId generates a command" <|
                \_ ->
                    let
                        modelWithTemp = { mockModel | tempClientId = "test-client" }
                        ( _, cmd ) =
                            update SaveClientId modelWithTemp
                    in
                    Expect.notEqual cmd Cmd.none
                    
            , test "StoredClientIdReceived updates clientId" <|
                \_ ->
                    let
                        ( model, _ ) =
                            update (StoredClientIdReceived "test-client") mockModel
                    in
                    Expect.equal model.clientId (Just "test-client")
            ]
        ]
