module State exposing (init, update, subscriptions)

import Data exposing (loadAppBootstrap, signOut)
import Events.State
import Homepage.State
import Navigation exposing (Location, newUrl)
import Profiles.State
import RemoteData exposing (RemoteData(..))
import Site exposing (pageToHash, hashToPage)
import Types exposing (..)


initialModel : Model
initialModel =
    { auth = NotSignedIn
    , page = HomePage
    , homepage = Homepage.State.initialModel
    , events = Events.State.initialModel
    , profiles = Profiles.State.initialModel
    , gitHubOAuthConfig = Loading
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            hashToPage location.hash

        changePageMsg =
            ChangePage page

        -- Let the update function create Cmds as required, depending on the
        -- initial view. For example, if the initial view is the Profiles page,
        -- we need to load the profiles.
        ( model, initialPageCmd ) =
            update changePageMsg initialModel
    in
        model ! (loadAppBootstrap :: [ initialPageCmd ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            -- leave the model untouched and issue a command to
            -- change the url, which then triggers ChangePage
            ( model, newUrl <| pageToHash page )

        ChangePage page ->
            let
                cmd =
                    case page of
                        ProfilesPage ->
                            Cmd.map ProfilesMsg Profiles.State.init

                        otherwise ->
                            Cmd.none
            in
                ( { model | page = page }, cmd )

        AppBootstrapResponse (Success appBootstrapResource) ->
            processAppBootstrap model appBootstrapResource

        AppBootstrapResponse err ->
            -- Ignore all other app bootstrap responses for now
            let
                _ =
                    Debug.log "AppBootstrap request failed" err
            in
                ( model, Cmd.none )

        SignOutClick ->
            ( model, signOut )

        SignOutResponse _ ->
            ( { model | auth = NotSignedIn }, Cmd.none )

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


processAppBootstrap : Model -> AppBootstrapResource -> ( Model, Cmd Msg )
processAppBootstrap model appBootstrapResource =
    let
        auth =
            case ( appBootstrapResource.signedIn, appBootstrapResource.profile ) of
                ( True, Just profile ) ->
                    SignedIn profile

                otherwise ->
                    NotSignedIn

        gitHubOAuthConfig =
            case appBootstrapResource.gitHubClientId of
                Just clientId ->
                    Success
                        { clientId = clientId
                        , redirectUrl = appBootstrapResource.gitHubOAuthRedirectUrl
                        }

                otherwise ->
                    Failure "No client ID"
    in
        ( { model
            | auth = auth
            , gitHubOAuthConfig = gitHubOAuthConfig
          }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
