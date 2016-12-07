module Homepage.Types exposing (..)

import Html exposing (Html)
import RemoteData exposing (WebData)


type alias Content =
    Html Msg


type alias Model =
    { content : WebData Content }


type Msg
    = ContentResponse (WebData Content)
