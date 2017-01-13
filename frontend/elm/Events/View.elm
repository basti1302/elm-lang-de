module Events.View exposing (view)

import Html exposing (..)
import Events.Types exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ text "Hier findet ihr bald aktuelle Termine zum Thema Elm im deutschsprachigen Raum. Stay tuned! :-)"
        ]
