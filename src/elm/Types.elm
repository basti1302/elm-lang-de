module Types exposing (..)

import Homepage.Types
import Events.Types
import Developers.Types


type Page
    = HomePage
    | EventsPage
    | DevelopersPage


type alias Model =
    { page : Page
    , homepage : Homepage.Types.Model
    , events : Events.Types.Model
    , developers : Developers.Types.Model
    }


type Msg
    = ChangePage Page
    | HomepageMsg Homepage.Types.Msg
    | EventsMsg Events.Types.Msg
    | DevelopersMsg Developers.Types.Msg
