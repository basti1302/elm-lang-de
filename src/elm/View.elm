module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList, href)
import Homepage.View
import Events.View
import Developers.View


view : Model -> Html Msg
view model =
    let
        pageContent =
            case model.page of
                HomePage ->
                    Html.map HomepageMsg
                        (Homepage.View.view model.homepage)

                EventsPage ->
                    Html.map EventsMsg
                        (Events.View.view model.events)

                DevelopersPage ->
                    Html.map DevelopersMsg
                        (Developers.View.view model.developers)
    in
        div
            [ class "overall" ]
            [ pageHeader model
            , div
                [ class "main" ]
                [ pageContent ]
            ]



-- LAYOUT (private)


pageHeader : Model -> Html Msg
pageHeader model =
    header
        [ class "header" ]
        [ div [ class "header__inner wrap" ]
            [ a
                [ class "header__title", href "/" ]
                [ text "Elm" ]
            , nav
                [ class "header__nav nav" ]
                [ navItem (model.page == EventsPage) EventsPage "Termine"
                , navItem (model.page == DevelopersPage) DevelopersPage "Entwickler"
                ]
            ]
        ]


navItem : Bool -> Page -> String -> Html Msg
navItem isCurrent page title =
    let
        classes =
            [ ( "nav__item", True )
            , ( "nav__item--current", isCurrent )
            ]
    in
        a
            [ href "#"
            , classList classes
            , onClick (ChangePage page)
            ]
            [ text title ]
