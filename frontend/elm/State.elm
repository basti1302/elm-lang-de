module State exposing (init, update, subscriptions)

import Data exposing (loadAppBootstrap)
import Events.State
import Homepage.State
import Navigation exposing (Location, newUrl)
import Profiles.State
import RemoteData exposing (RemoteData(..))
import Site exposing (pageToHash, hashToPage)
import Types exposing (..)


initialModel : Page -> Model
initialModel page =
    { auth = NotSignedIn
    , page = page
    , homepage = Homepage.State.initialModel
    , events = Events.State.initialModel
    , profiles = Profiles.State.initialModel
    , gitHubClientId = Nothing
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            hashToPage location.hash
    in
        ( initialModel page, loadAppBootstrap )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            -- leave the model untouched and issue a command to
            -- change the url, which then triggers ChangePage
            ( model, newUrl <| pageToHash page )

        AppBootstrapResponse (Success appBootstrapResource) ->
            let
                auth =
                    case ( appBootstrapResource.signedIn, appBootstrapResource.profile ) of
                        ( True, Just profile ) ->
                            SignedIn profile

                        otherwise ->
                            NotSignedIn
            in
                ( { model
                    | auth = auth
                    , gitHubClientId = appBootstrapResource.gitHubClientId
                  }
                , Cmd.none
                )

        AppBootstrapResponse err ->
            -- Ignore all other app bootstrap responses for now
            let
                _ =
                    Debug.log "AppBootstrap request failed" err
            in
                ( model, Cmd.none )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        HomepageMsg homeMsg ->
            let
                ( homepageModel, cmd ) =
                    Homepage.State.update homeMsg model.homepage
            in
                ( { model | homepage = homepageModel }
                , Cmd.map HomepageMsg cmd
                )

        EventsMsg eventMsg ->
            let
                ( eventsModel, cmd ) =
                    Events.State.update eventMsg model.events
            in
                ( { model | events = eventsModel }
                , Cmd.map EventsMsg cmd
                )

        ProfilesMsg profileMsg ->
            let
                ( profilesModel, cmd ) =
                    Profiles.State.update profileMsg model.profiles
            in
                ( { model | profiles = profilesModel }
                , Cmd.map ProfilesMsg cmd
                )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
