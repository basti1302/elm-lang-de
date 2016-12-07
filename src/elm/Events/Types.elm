module Events.Types exposing (..)

import Date exposing (Date)
import Html exposing (Html)
import RemoteData exposing (WebData)


type alias Content =
    Html Msg


type alias Event =
    { date : Date
    , title : String
    , url : String
    }


type alias Model =
    { events : List Event
    , content : WebData Content
    }


type Msg
    = ContentResponse (WebData Content)
