module Homepage.Types exposing (..)

import GitHubOAuthConfig exposing (GitHubOAuthConfig)
import RemoteData exposing (RemoteData(..))


type alias Model =
    { gitHubOAuthConfig : RemoteData String GitHubOAuthConfig
    }


type Msg
    = NoOp
