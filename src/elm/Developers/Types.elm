module Developers.Types exposing (..)


type alias Developer =
    String


type alias Model =
    { developers : List Developer }


type Msg
    = NoOp
