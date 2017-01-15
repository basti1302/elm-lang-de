module Profiles.State exposing (initialModel, update)

import Profiles.Types exposing (..)
import Profiles.Data exposing (loadProfileDetails, loadProfileList)
import RemoteData exposing (RemoteData(..))


initialModel : Model
initialModel =
    { page = ListPage
    , profiles = NotAsked
    , currentProfile = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            case page of
                ListPage ->
                    -- TODO Only load profile list if it has not been loaded
                    -- yet, or might be outdated (cache it client side and/or
                    -- use ETags and If-None-Match header in HTTP
                    -- response/request). The ETag of the profile list could be
                    -- the latest modified date of any profile.
                    ( model, loadProfileList )

                DetailsPage urlFragment ->
                    ( model, loadProfileDetails urlFragment )

        ProfileDetailsResponse (Success profile) ->
            ( { model
                | currentProfile = Just profile
                , page = DetailsPage profile.urlFragment
              }
            , Cmd.none
            )

        ProfileDetailsResponse noSuccess ->
            let
                _ =
                    Debug.log "Could not load profile." noSuccess
            in
                ( model, Cmd.none )

        ProfileListResponse profiles ->
            ( { model
                | profiles = profiles
                , page = ListPage
                , currentProfile = Nothing
              }
            , Cmd.none
            )
