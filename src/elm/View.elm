module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href)


view : Model -> Html Msg
view model =
    div
        [ class "main" ]
        [ pageHeader ]



-- LAYOUT (private)


pageHeader : Html Msg
pageHeader =
    let
        isCurrent =
            False

        navItems =
            List.map (navItem isCurrent "#") navigation
    in
        header
            [ class "header" ]
            [ div [ class "header__inner wrap" ]
                [ a
                    [ class "header__title", href "/" ]
                    [ text "Elm" ]
                , nav
                    [ class "header__nav nav" ]
                    navItems
                ]
            ]


navigation : List String
navigation =
    [ "Termine", "Entwickler" ]


navItem : Bool -> String -> String -> Html Msg
navItem isCurrent url title =
    let
        classes =
            [ ( "nav__item", True ), ( "nav__item--current", isCurrent ) ]
    in
        a
            [ href url
            , classList classes
            ]
            [ text title ]
