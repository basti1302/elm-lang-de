module Events.Types exposing (..)

import Date exposing (Date)


type alias Event =
    { date : Date
    , title : String
    , url : String
    }


type alias Model =
    { events : List Event }


type Msg
    = NoOp
