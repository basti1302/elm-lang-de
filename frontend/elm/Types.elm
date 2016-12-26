module Types exposing (..)

import Homepage.Types
import Events.Types
import Developers.Types
import RemoteData exposing (WebData)


type Page
    = HomePage
    | EventsPage
    | DevelopersPage
    | NotFound


type Msg
    = AppBootstrapResponse (WebData AppBootstrapResource)
    | ChangePage Page
    | DevelopersMsg Developers.Types.Msg
    | EventsMsg Events.Types.Msg
    | HomepageMsg Homepage.Types.Msg
    | Navigate Page


type alias Model =
    { page : Page
    , homepage : Homepage.Types.Model
    , events : Events.Types.Model
    , developers : Developers.Types.Model
    , gitHubClientId : Maybe String
    }


type alias AppBootstrapResource =
    { gitHubClientId : Maybe String
    }
