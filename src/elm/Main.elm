module Main exposing (..)

import Navigation
import Site exposing (locationToMsg)
import State exposing (init, update, subscriptions)
import Types exposing (Flags, Model, Msg)
import View exposing (view)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags locationToMsg
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
