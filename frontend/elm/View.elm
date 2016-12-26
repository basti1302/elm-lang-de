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

                NotFound ->
                    div [] [ text "404" ]
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
    let
        navLink =
            navItem model.page
    in
        header
            [ class "header" ]
            [ nav
                [ class "nav" ]
                [ navLink HomePage "Elm"
                , navLink EventsPage "Termine"
                , navLink DevelopersPage "Entwickler"
                , signInWithGitHub model
                ]
            ]


navItem : Page -> Page -> String -> Html Msg
navItem currentPage page title =
    let
        classes =
            [ ( "nav__item", True )
            , ( "nav__item--current", currentPage == page )
            ]
    in
        a
            [ classList classes
            , onClick (Navigate page)
            ]
            [ text title ]


signInWithGitHub : Model -> Html Msg
signInWithGitHub model =
    -- TODO Replace this with properly styled "sign in with github" button that is
    -- only displayed after we have received a client id. Something the log in pop
    -- up on angularjs.de would be nice.
    -- TODO Check if the user is actually already signed in to GitHub.
    case model.gitHubClientId of
        Just clientId ->
            let
                -- TODO Fetch redirect URL from app bootstrap, too?
                redirectUrl =
                    "http://localhost:8080/oauth/github"

                gitHubUrl =
                    "https://github.com/login/oauth/authorize?client_id=" ++ clientId ++ "&scope=user:email&redirect_uri=" ++ redirectUrl
            in
                a
                    [ class "nav__item"
                    , href gitHubUrl
                    ]
                    [ text "Mit GitHub anmelden" ]

        otherwise ->
            text "Anmelden mit GitHub zur Zeit nicht verfügbar."
