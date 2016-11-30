module Main exposing (..)

import Types exposing (Model, Msg)
import State exposing (init, update, subscriptions)
import View exposing (view)
import Site exposing (locationToMsg)
import Navigation


main : Program Never Model Msg
main =
    Navigation.program locationToMsg
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
