module Types
    exposing
        ( AppBootstrapResource
        , AuthenticationState(..)
        , Model
        , Msg(..)
        , Page(..)
        , SignedInModel
        , editProfileTranslator
        )

import EditProfile.Translator
import EditProfile.Types
import Events.Types
import GitHubOAuthConfig exposing (GitHubOAuthConfig)
import Homepage.Types
import Http
import Notification exposing (..)
import Profiles.Types exposing (Profile)
import RemoteData exposing (RemoteData, WebData)


type alias Model =
    { auth : AuthenticationState
    , page : Page
    , homepage : Homepage.Types.Model
    , events : Events.Types.Model
    , profiles : Profiles.Types.Model
    , gitHubOAuthConfig : RemoteData String GitHubOAuthConfig
    , notification : Maybe Notification
    , showSmallScreenNav : Bool
    }


type AuthenticationState
    = SignedIn SignedInModel
    | NotSignedIn


type alias SignedInModel =
    { profile : Profiles.Types.Profile
    , editProfileModel : EditProfile.Types.Model
    , showProfilePopupMenu : Bool
    }


type Msg
    = AppBootstrapResponse (WebData AppBootstrapResource)
    | ChangePage Page
    | CloseAllPopups
    | CloseNotification
    | CloseProfilePopupMenu
    | EditProfileMsg EditProfile.Types.InternalMsg
    | EventsMsg Events.Types.Msg
    | HomepageMsg Homepage.Types.Msg
    | Navigate Page
    | ProfilesMsg Profiles.Types.Msg
    | SignOutClick
    | SignOutResponse (Result Http.Error ())
    | ShowNotification Notification
    | ToggleSmallScreenNav
    | ToggleProfilePopupMenu


type Page
    = HomePage
    | EventsPage
    | EditProfilePage
    | ImprintPage
    | ProfilesPage Profiles.Types.Page
    | NotFoundPage


type alias AppBootstrapResource =
    { signedIn : Bool
    , profile : Maybe Profile
    , gitHubClientId : Maybe String
    , gitHubOAuthRedirectUrl : String
    }


editProfileTranslator : EditProfile.Translator.Translator Msg
editProfileTranslator =
    let
        editProfileTranslationDictionary =
            { onInternalMessage = EditProfileMsg
            , onShowNotification = ShowNotification
            }
    in
        EditProfile.Translator.translator editProfileTranslationDictionary
