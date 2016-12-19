module Developers.State exposing (init, initialModel, update, subscriptions)

import Developers.Types exposing (..)


initialModel : Model
initialModel =
    { developers = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
