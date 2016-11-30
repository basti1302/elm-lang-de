module Developers.View exposing (view)

import Html exposing (..)
import Developers.Types exposing (..)


view : Model -> Html Msg
view model =
    div [] [ text "Entwickler" ]
