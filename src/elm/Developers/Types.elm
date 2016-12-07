module Developers.Types exposing (..)

import Html exposing (Html)
import RemoteData exposing (WebData)


type alias Developer =
    String


type alias Model =
    { developers : List Developer
    , content : WebData (Html Msg)
    }


type Msg
    = NoOp
