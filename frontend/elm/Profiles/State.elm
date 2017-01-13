module Profiles.State exposing (init, initialModel, update)

import Profiles.Types exposing (..)
import Profiles.Data exposing (loadProfiles)
import RemoteData exposing (RemoteData(..))


-- TODO Also load the list when the initial request is to #developers


initialModel : Model
initialModel =
    { profiles = NotAsked
    }


init : Cmd Msg
init =
    loadProfiles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProfilesResponse data ->
            ( { model | profiles = data }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
