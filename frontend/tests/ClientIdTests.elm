module ClientIdTests exposing (suite)

import Expect
import Main.Internal exposing (update)
import Msg exposing (Msg(..))
import Test exposing (..)
import TestHelpers exposing (mockModel, mockShortUrl)
import Types exposing (..)
import View.ClientDashboard
import Http
import Test.Html.Query as Query
import Test.Html.Selector as Selector

{-
   Tests specifically for client ID functionality
-}

suite : Test
suite =
    describe "Client ID Functionality"
        [ describe "Client ID Update Logic"
            [ test "ClientIdChanged updates tempClientId" <|
                \_ ->
                    let
                        ( updatedModel, _ ) =
                            update (ClientIdChanged "test-client") mockModel
                    in
                    Expect.equal updatedModel.tempClientId "test-client"
                    
            , test "SaveClientId sets clientId from tempClientId" <|
                \_ ->
                    let
                        modelWithTemp = { mockModel | tempClientId = "test-client" }
                        ( updatedModel, _ ) =
                            update SaveClientId modelWithTemp
                    in
                    Expect.equal updatedModel.clientId (Just "test-client")
                    
            , test "SaveClientId with empty tempClientId shows warning" <|
                \_ ->
                    let
                        ( updatedModel, _ ) =
                            update SaveClientId mockModel
                    in
                    case updatedModel.notification of
                        Just notification ->
                            Expect.equal notification.notificationType Warning
                        Nothing ->
                            Expect.fail "Expected warning notification"
                            
            , test "StoredClientIdReceived sets clientId" <|
                \_ ->
                    let
                        ( updatedModel, _ ) =
                            update (StoredClientIdReceived "saved-client") mockModel
                    in
                    Expect.equal updatedModel.clientId (Just "saved-client")
                    
            , test "LoadClientUrls without clientId shows warning" <|
                \_ ->
                    let
                        ( updatedModel, _ ) =
                            update LoadClientUrls mockModel
                    in
                    case updatedModel.notification of
                        Just notification ->
                            Expect.equal notification.notificationType Warning
                        Nothing ->
                            Expect.fail "Expected warning notification"
                            
            , test "LoadClientUrls with clientId sets isLoading" <|
                \_ ->
                    let
                        modelWithClientId = { mockModel | clientId = Just "test-client" }
                        ( updatedModel, _ ) =
                            update LoadClientUrls modelWithClientId
                    in
                    Expect.equal updatedModel.isLoading True
            ]
            
        , describe "Client ID in API Requests"
            [ test "ClientUrlsLoaded success updates urls list" <|
                \_ ->
                    let
                        ( updatedModel, _ ) =
                            update (ClientUrlsLoaded (Ok [mockShortUrl])) mockModel
                    in
                    Expect.equal (List.length updatedModel.urls) 1
                    
            , test "ClientUrlsLoaded error shows notification" <|
                \_ ->
                    let
                        ( updatedModel, _ ) =
                            update (ClientUrlsLoaded (Err (Http.BadStatus 404))) mockModel
                    in
                    case updatedModel.notification of
                        Just notification ->
                            Expect.equal notification.notificationType Error
                        Nothing ->
                            Expect.fail "Expected error notification"
            ]
            
        , describe "Client Dashboard View"
            [ test "Client dashboard view includes client ID input" <|
                \_ ->
                    let
                        dashboard = View.ClientDashboard.viewClientDashboard mockModel
                                    |> TestHelpers.elementToHtml
                                    |> Query.fromHtml
                    in
                    dashboard
                        |> Query.has [ Selector.text "Your Client ID" ]
                        
            , test "Client dashboard shows dashboard header" <|
                \_ ->
                    let
                        dashboard = View.ClientDashboard.viewClientDashboard mockModel
                                    |> TestHelpers.elementToHtml
                                    |> Query.fromHtml
                    in
                    dashboard
                        |> Query.has [ Selector.text "Your URL Dashboard" ]
                        
            , test "Client dashboard with no URLs shows empty state" <|
                \_ ->
                    let
                        dashboard = View.ClientDashboard.viewClientDashboard mockModel
                                    |> TestHelpers.elementToHtml
                                    |> Query.fromHtml
                    in
                    dashboard
                        |> Query.has [ Selector.text "No URLs Found" ]
                        
            , test "Client dashboard with URLs shows URL list" <|
                \_ ->
                    let
                        modelWithUrlsAndClient = 
                            { mockModel 
                            | urls = [mockShortUrl]
                            , clientId = Just "test-client" 
                            }
                            
                        dashboard = View.ClientDashboard.viewClientDashboard modelWithUrlsAndClient
                                    |> TestHelpers.elementToHtml
                                    |> Query.fromHtml
                    in
                    dashboard
                        |> Query.has [ Selector.text mockShortUrl.shortCode ]
            ]
        ]
