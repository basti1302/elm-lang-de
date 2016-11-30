module Events.View exposing (view)

import Html exposing (..)
import Events.Types exposing (..)


view : Model -> Html Msg
view model =
    div [] [ text "Termine" ]
