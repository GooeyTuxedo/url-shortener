module TestableNavigation exposing (Key(..), back, dummy, forward, fromReal, load, pushUrl, replaceUrl)

{-| A wrapper for Browser.Navigation that allows for testing.

In production code, use the actual Nav.Key. In tests, use the dummy.

-}

import Browser.Navigation as Nav


type Key
    = RealKey Nav.Key
    | DummyKey


dummy : Key
dummy =
    DummyKey


fromReal : Nav.Key -> Key
fromReal navKey =
    RealKey navKey


pushUrl : Key -> String -> Cmd msg
pushUrl key url =
    case key of
        RealKey navKey ->
            Nav.pushUrl navKey url

        DummyKey ->
            Cmd.none


load : String -> Cmd msg
load =
    Nav.load


back : Key -> Int -> Cmd msg
back key n =
    case key of
        RealKey navKey ->
            Nav.back navKey n

        DummyKey ->
            Cmd.none


forward : Key -> Int -> Cmd msg
forward key n =
    case key of
        RealKey navKey ->
            Nav.forward navKey n

        DummyKey ->
            Cmd.none


replaceUrl : Key -> String -> Cmd msg
replaceUrl key url =
    case key of
        RealKey navKey ->
            Nav.replaceUrl navKey url

        DummyKey ->
            Cmd.none
