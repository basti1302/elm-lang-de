module EditProfile.State exposing (initialModel, update)

import EditProfile.Data exposing (updateProfile)
import EditProfile.Types exposing (..)
import Http
import Json.Decode as Decode
import Navigation
import Notification
import Profiles.Types exposing (Profile)
import RemoteData exposing (RemoteData(..))
import Routes
import Time
import Types as MainTypes
import Util.CmdHelper as CmdHelper
import Util.HttpHelper as HttpHelper


initialModel : Profile -> Model
initialModel profile =
    { profile = profile
    , showBiographyPreview = False
    }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            updateInProfile model setName name

        Job job ->
            updateInProfile model setJob job

        Bio bio ->
            updateInProfile model setBio bio

        City city ->
            updateInProfile model setCity city

        Country country ->
            updateInProfile model setCountry country

        EMail email ->
            updateInProfile model setEMail email

        HomePage homepage ->
            updateInProfile model setHomepage homepage

        GitHubUsername username ->
            updateInProfile model setGitHubUsername username

        TwitterHandle handle ->
            updateInProfile model setTwitterHandle handle

        Available available ->
            updateBooleanInProfile model setAvailable available

        SwitchToBiographyEdit ->
            ( { model | showBiographyPreview = False }, Cmd.none )

        SwitchToBiographyPreview ->
            ( { model | showBiographyPreview = True }, Cmd.none )

        UpdateProfile ->
            ( model, updateProfile model.profile )

        UpdateProfileResponse (Success profile) ->
            let
                showPopupCmd =
                    "Dein Profil wurde gespeichert"
                        |> Notification.infoWithTimeout (4 * Time.second)
                        |> ShowNotification
                        |> ForParent
                        |> CmdHelper.msgToCmd

                goToProfileCmd =
                    profile.urlFragment
                        |> Profiles.Types.DetailsPage
                        |> MainTypes.ProfilesPage
                        |> Routes.pageToHash
                        |> Navigation.newUrl
            in
                { model
                    | profile = profile
                }
                    ! [ showPopupCmd, goToProfileCmd ]

        UpdateProfileResponse (Failure httpError) ->
            -- TODO Converting a bad request response with validation messages
            -- into user visible error notifications should be a reusable
            -- helper function in Util.HttpHelper, this code should not be here.
            case httpError of
                Http.BadStatus { body } ->
                    let
                        decodeResult =
                            Decode.decodeString HttpHelper.decodeMessages body
                    in
                        case decodeResult of
                            Ok { messages } ->
                                let
                                    cmds =
                                        messagesToPopupCommands messages
                                in
                                    model ! cmds

                            Err err ->
                                ( model, genericErrorNotificationCmd )

                otherwise ->
                    ( model, genericErrorNotificationCmd )

        UpdateProfileResponse neitherSuccessNorFailure ->
            let
                _ =
                    Debug.log "Ignoring message" neitherSuccessNorFailure
            in
                ( model, Cmd.none )


updateInProfile : Model -> (Profile -> String -> Profile) -> String -> ( Model, Cmd Msg )
updateInProfile model updateFn newValue =
    let
        profile =
            model.profile

        updatedProfile =
            updateFn profile newValue
    in
        ( { model | profile = updatedProfile }, Cmd.none )


updateBooleanInProfile : Model -> (Profile -> Bool -> Profile) -> Bool -> ( Model, Cmd Msg )
updateBooleanInProfile model updateFn newValue =
    let
        profile =
            model.profile

        updatedProfile =
            updateFn profile newValue
    in
        ( { model | profile = updatedProfile }, Cmd.none )


messagesToPopupCommands : List String -> List (Cmd Msg)
messagesToPopupCommands messages =
    -- TODO Multiple notification Cmds can be created here, but only the last one
    -- will be visible :-(
    if List.isEmpty messages then
        [ genericErrorNotificationCmd ]
    else
        let
            messageToCmd message =
                message
                    |> Notification.errorWithTimeout (4 * Time.second)
                    |> ShowNotification
                    |> ForParent
                    |> CmdHelper.msgToCmd
        in
            List.map messageToCmd messages


genericErrorNotificationCmd : Cmd Msg
genericErrorNotificationCmd =
    "Das hat leider nicht geklappt :-("
        |> Notification.errorWithTimeout (4 * Time.second)
        |> ShowNotification
        |> ForParent
        |> CmdHelper.msgToCmd


setName : Profile -> String -> Profile
setName profile name =
    { profile | name = name }


setJob : Profile -> String -> Profile
setJob profile job =
    { profile | job = job }


setBio : Profile -> String -> Profile
setBio profile bio =
    { profile | bio = bio }


setCity : Profile -> String -> Profile
setCity profile city =
    { profile | city = city }


setCountry : Profile -> String -> Profile
setCountry profile country =
    { profile | country = country }


setEMail : Profile -> String -> Profile
setEMail profile email =
    { profile | email = email }


setHomepage : Profile -> String -> Profile
setHomepage profile homepage =
    { profile | homepage = homepage }


setGitHubUsername : Profile -> String -> Profile
setGitHubUsername profile gitHubUsername =
    { profile | gitHubUsername = gitHubUsername }


setTwitterHandle : Profile -> String -> Profile
setTwitterHandle profile twitterHandle =
    { profile | twitterHandle = twitterHandle }


setAvailable : Profile -> Bool -> Profile
setAvailable profile available =
    { profile | available = available }
