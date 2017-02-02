module State exposing (init, update, subscriptions)

import Data exposing (loadAppBootstrap, signOut)
import EditProfile.State
import EditProfile.Types
import Events.State
import Homepage.State
import Navigation exposing (Location)
import Notification
import Profiles.State
import Profiles.Types exposing (Profile)
import RemoteData exposing (RemoteData(..))
import Routes
import Types exposing (..)
import Util.CmdHelper as CmdHelper


initialModel : Model
initialModel =
    { auth = NotSignedIn
    , page = HomePage
    , homepage = Homepage.State.initialModel
    , events = Events.State.initialModel
    , profiles = Profiles.State.initialModel
    , gitHubOAuthConfig = Loading
    , notification = Nothing
    , showSmallScreenNav = False
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            Routes.hashToPage location.hash

        changePageMsg =
            ChangePage page

        -- Let the update function create Cmds as required, depending on the
        -- initial view. For example, if the initial view is the Profiles page,
        -- we need to load the profiles.
        ( model, initialPageCmd ) =
            update changePageMsg initialModel
    in
        model ! (loadAppBootstrap :: [ initialPageCmd ])


initSignedInModel : Profile -> AuthenticationState
initSignedInModel profile =
    SignedIn
        { profile = profile
        , editProfileModel = EditProfile.State.initialModel profile
        , showProfilePopupMenu = False
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppBootstrapResponse (Success appBootstrapResource) ->
            processAppBootstrapResponse model appBootstrapResource

        AppBootstrapResponse err ->
            -- Ignore all other app bootstrap responses for now
            let
                _ =
                    Debug.log "AppBootstrap request failed" err
            in
                ( model, Cmd.none )

        ChangePage page ->
            let
                ( updatedModel, cmd ) =
                    case page of
                        -- TODO Right now, the profile list is loaded *every*
                        -- time the user clicks on a link that leads to an URL
                        -- like "#developers.*". Do we want that?
                        ProfilesPage subPage ->
                            let
                                subMsg =
                                    Profiles.Types.ChangePage subPage
                                        |> ProfilesMsg
                            in
                                update subMsg model

                        otherwise ->
                            ( model, Cmd.none )
            in
                ( { updatedModel | page = page }, cmd )

        CloseAllPopups ->
            let
                smallScreenNavClose =
                    { model | showSmallScreenNav = False }
            in
                update CloseProfilePopupMenu smallScreenNavClose

        CloseNotification ->
            ( { model | notification = Nothing }, Cmd.none )

        CloseProfilePopupMenu ->
            updateSignedIn msg closeProfilePopupMenu model

        EditProfileMsg editProfileMsg ->
            updateSignedIn msg (processEditProfileMsg editProfileMsg) model

        EventsMsg eventMsg ->
            let
                ( eventsModel, cmd ) =
                    Events.State.update eventMsg model.events
            in
                ( { model | events = eventsModel }
                , Cmd.map EventsMsg cmd
                )

        HomepageMsg homeMsg ->
            let
                ( homepageModel, cmd ) =
                    Homepage.State.update homeMsg model.homepage
            in
                ( { model | homepage = homepageModel }
                , Cmd.map HomepageMsg cmd
                )

        Navigate page ->
            -- leave the model untouched and issue a command to
            -- change the url, which then triggers ChangePage
            ( model, Navigation.newUrl <| Routes.pageToHash page )

        ProfilesMsg profileMsg ->
            let
                ( profilesModel, cmd ) =
                    Profiles.State.update profileMsg model.profiles
            in
                ( { model | profiles = profilesModel }
                , Cmd.map ProfilesMsg cmd
                )

        SignOutClick ->
            ( model, signOut )

        SignOutResponse _ ->
            let
                -- If necessary, redirect user to a page that does not require
                -- xyr to be signed in.
                newPage =
                    if model.page == EditProfilePage then
                        ProfilesPage Profiles.Types.ListPage
                    else
                        model.page
            in
                ( { model | auth = NotSignedIn, page = newPage }, Cmd.none )

        ShowNotification notification ->
            let
                timeoutCmd =
                    case Notification.getTimeout notification of
                        Just timeout ->
                            CmdHelper.delay timeout CloseNotification

                        Nothing ->
                            Cmd.none
            in
                ( { model | notification = Just notification }, timeoutCmd )

        ToggleSmallScreenNav ->
            ( { model | showSmallScreenNav = not model.showSmallScreenNav }
            , Cmd.none
            )

        ToggleProfilePopupMenu ->
            updateSignedIn msg toggleProfilePopupMenu model


updateSignedIn :
    Msg
    -> (SignedInModel -> ( AuthenticationState, Cmd Msg ))
    -> Model
    -> ( Model, Cmd Msg )
updateSignedIn msg updateFn model =
    case model.auth of
        NotSignedIn ->
            -- Ignore messages that require a signed in user when not signed in
            let
                _ =
                    Debug.log "Ignoring message while not signed in" msg
            in
                ( model, Cmd.none )

        SignedIn signedInModel ->
            let
                ( updatedAuth, cmd ) =
                    updateFn signedInModel
            in
                ( { model | auth = updatedAuth }, cmd )


closeProfilePopupMenu : SignedInModel -> ( AuthenticationState, Cmd Msg )
closeProfilePopupMenu signedInModel =
    SignedIn
        { profile = signedInModel.profile
        , editProfileModel = signedInModel.editProfileModel
        , showProfilePopupMenu = False
        }
        ! []


toggleProfilePopupMenu : SignedInModel -> ( AuthenticationState, Cmd Msg )
toggleProfilePopupMenu signedInModel =
    SignedIn
        { profile = signedInModel.profile
        , editProfileModel = signedInModel.editProfileModel
        , showProfilePopupMenu = not signedInModel.showProfilePopupMenu
        }
        ! []


processEditProfileMsg :
    EditProfile.Types.InternalMsg
    -> SignedInModel
    -> ( AuthenticationState, Cmd Msg )
processEditProfileMsg editProfileMsg signedInModel =
    let
        ( updatedEditProfileModel, cmd ) =
            EditProfile.State.update editProfileMsg signedInModel.editProfileModel
    in
        SignedIn
            { profile = updatedEditProfileModel.profile
            , editProfileModel = updatedEditProfileModel
            , showProfilePopupMenu = signedInModel.showProfilePopupMenu
            }
            ! [ Cmd.map editProfileTranslator cmd ]


processAppBootstrapResponse : Model -> AppBootstrapResource -> ( Model, Cmd Msg )
processAppBootstrapResponse model appBootstrapResource =
    let
        auth =
            case ( appBootstrapResource.signedIn, appBootstrapResource.profile ) of
                ( True, Just profile ) ->
                    initSignedInModel profile

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

        oldHomepageModel =
            model.homepage

        newHomepageModel =
            { oldHomepageModel | gitHubOAuthConfig = gitHubOAuthConfig }
    in
        ( { model
            | auth = auth
            , gitHubOAuthConfig = gitHubOAuthConfig
            , homepage = newHomepageModel
          }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
