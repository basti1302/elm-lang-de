module Developers.Types exposing (..)


type alias Developer =
    { name : String
    , urlFragment : String
    , profileMarkdown : String
    }


type Page
    = DeveloperListPage
    | DeveloperDetailsPage String


type alias Model =
    { page : Page
    , developers : List Developer
    , developer : Maybe Developer
    }


type Msg
    = Navigate Page
    | ChangePage Page
    | NoOp
