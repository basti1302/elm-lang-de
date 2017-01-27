module Homepage.State
    exposing
        ( init
        , initialModel
        , update
        , subscriptions
        )

import Homepage.Types exposing (..)
import RemoteData exposing (RemoteData(NotAsked))


initialModel : Model
initialModel =
    { gitHubOAuthConfig = NotAsked }


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
