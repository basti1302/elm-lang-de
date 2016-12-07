module Homepage.State exposing (init, initialModel, update, subscriptions)

import RemoteData exposing (..)
import Homepage.Types exposing (..)
import Homepage.Data exposing (loadContent)


initialModel : Model
initialModel =
    { content = NotAsked }


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
