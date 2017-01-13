module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Homepage.View
import Events.View
import Profiles.View
import RemoteData exposing (RemoteData(..))


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

                ProfilesPage ->
                    Html.map ProfilesMsg
                        (Profiles.View.view model.profiles)

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
                , navLink ProfilesPage "Entwickler"
                , authentication model
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


authentication : Model -> Html Msg
authentication model =
    -- TODO Clean this up :)
    -- TODO Replace this with properly styled "sign in with github" button that is
    -- only displayed after we have received a client id. Something like the
    -- log in pop up on angularjs.de would be nice.
    -- TODO Sign out (requires backend call, deletes github-access-code cookie)
    case model.auth of
        SignedIn profile ->
            let
                profileComponents =
                    if String.isEmpty profile.gitHubAvatarUrl then
                        [ text profile.name ]
                    else
                        [ img [ src profile.gitHubAvatarUrl ] []
                        , text profile.name
                        ]
            in
                div [ class "signed-in" ] profileComponents

        NotSignedIn ->
            case model.gitHubOAuthConfig of
                Success gitHubOAuthConfig ->
                    let
                        redirectUrl =
                            gitHubOAuthConfig.redirectUrl

                        clientId =
                            gitHubOAuthConfig.clientId

                        gitHubUrl =
                            "https://github.com/login/oauth/authorize?client_id="
                                ++ clientId
                                ++ "&redirect_uri="
                                ++ redirectUrl
                    in
                        a
                            [ class "nav__item"
                            , href gitHubUrl
                            ]
                            [ text "Mit GitHub anmelden" ]

                otherwise ->
                    text "..."
