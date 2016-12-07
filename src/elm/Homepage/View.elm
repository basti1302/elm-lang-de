module Homepage.View exposing (view)

import Html exposing (Html, text)
import RemoteData exposing (..)
import Homepage.Types exposing (..)


view : Model -> Html Msg
view model =
    case model.content of
        NotAsked ->
            text "Einen Moment …"

        Loading ->
            text "Wird geladen …"

        Failure err ->
            text ("Fehler: " ++ toString err)

        Success html ->
            html
