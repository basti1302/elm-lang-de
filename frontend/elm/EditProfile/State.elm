module EditProfile.State exposing (initialModel, update)

import EditProfile.Data exposing (updateProfile)
import EditProfile.Types exposing (..)
import Profiles.Types exposing (Profile)
import RemoteData exposing (RemoteData(..))


initialModel : Profile -> Model
initialModel profile =
    { profile = profile
    , showBiographyPreview = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
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
            ( { model
                | profile = profile
              }
            , Cmd.none
            )

        UpdateProfileResponse noSuccess ->
            let
                _ =
                    Debug.log "Could not update profile." noSuccess
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
