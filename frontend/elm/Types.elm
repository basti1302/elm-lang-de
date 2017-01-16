module Types exposing (..)

import Http
import Homepage.Types
import EditProfile.Types
import Events.Types
import Profiles.Types exposing (Profile)
import RemoteData exposing (RemoteData, WebData)


type Page
    = HomePage
    | EventsPage
    | EditProfilePage
    | ProfilesPage Profiles.Types.Page
    | NotFound


type Msg
    = AppBootstrapResponse (WebData AppBootstrapResource)
    | ChangePage Page
    | EditProfileMsg EditProfile.Types.Msg
    | EventsMsg Events.Types.Msg
    | HomepageMsg Homepage.Types.Msg
    | Navigate Page
    | ProfilesMsg Profiles.Types.Msg
    | SignOutClick
    | SignOutResponse (Result Http.Error ())


type alias GitHubOAuthConfig =
    { clientId : String
    , redirectUrl : String
    }


type alias Model =
    { auth : AuthenticationState
    , page : Page
    , homepage : Homepage.Types.Model
    , events : Events.Types.Model
    , profiles : Profiles.Types.Model
    , gitHubOAuthConfig : RemoteData String GitHubOAuthConfig
    }


type AuthenticationState
    = SignedIn Profiles.Types.Profile
    | NotSignedIn


type alias AppBootstrapResource =
    { signedIn : Bool
    , profile : Maybe Profile
    , gitHubClientId : Maybe String
    , gitHubOAuthRedirectUrl : String
    }
