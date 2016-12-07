module Events.State exposing (init, initialModel, update, subscriptions)

import RemoteData exposing (..)
import Events.Types exposing (..)
import Events.Data exposing (loadContent)


initialModel : Model
initialModel =
    { events = []
    , content = NotAsked
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, loadContent )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ContentResponse response ->
            ( { model | content = response }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
