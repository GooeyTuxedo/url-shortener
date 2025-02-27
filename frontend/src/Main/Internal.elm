module Main.Internal exposing (update)

import Main as Main
import Msg exposing (Msg)
import Types exposing (Model)

{-| Re-export the update function from Main
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update =
    Main.update
