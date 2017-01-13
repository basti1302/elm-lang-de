module Events.State exposing (init, initialModel, update)

import Events.Types exposing (..)


initialModel : Model
initialModel =
    { events = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
