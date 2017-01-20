module EditProfile.Types exposing (..)

import RemoteData exposing (WebData)
import Profiles.Types exposing (Profile)


type alias Model =
    { profile : Profile
    , showBiographyPreview : Bool
    }


type Msg
    = Name String
    | Job String
    | Bio String
    | City String
    | Country String
    | EMail String
    | HomePage String
    | GitHubUsername String
    | TwitterHandle String
    | Available Bool
    | SwitchToBiographyEdit
    | SwitchToBiographyPreview
    | UpdateProfile
    | UpdateProfileResponse (WebData Profile)
