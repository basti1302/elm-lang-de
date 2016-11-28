module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, href)


view : Model -> Html Msg
view model =
    div
        [ class "main" ]
        [ pageHeader ]



-- LAYOUT (private)


pageHeader : Html Msg
pageHeader =
    header
        [ class "header" ]
        [ h1
            [ class "header__title" ]
            [ text "Elm" ]
        ]
