module Types exposing (..)

import Http
import Homepage.Types
import EditProfile.Types
import Events.Types
import GitHubOAuthConfig exposing (GitHubOAuthConfig)
import Profiles.Types exposing (Profile)
import RemoteData exposing (RemoteData, WebData)


type alias Model =
    { auth : AuthenticationState
    , page : Page
    , homepage : Homepage.Types.Model
    , events : Events.Types.Model
    , profiles : Profiles.Types.Model
    , gitHubOAuthConfig : RemoteData String GitHubOAuthConfig
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
    | EditProfileMsg EditProfile.Types.Msg
    | EventsMsg Events.Types.Msg
    | HomepageMsg Homepage.Types.Msg
    | Navigate Page
    | CloseAllPopups
    | ProfilesMsg Profiles.Types.Msg
    | SignOutClick
    | SignOutResponse (Result Http.Error ())
    | CloseProfilePopupMenu
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
