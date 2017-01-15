module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Homepage.View
import Events.View
import Profiles.View
import Profiles.Types
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

                ProfilesPage _ ->
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
                , navLink (ProfilesPage Profiles.Types.ListPage) "Entwickler"
                , authentication model
                ]
            ]


navItem : Page -> Page -> String -> Html Msg
navItem currentPage page title =
    -- TODO Shouldn't navItem produce simple links with the proper #anchor as
    -- href? We might be able to drop the Navigate messages and Routes#pageToHash
    -- completely.
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
    -- TODO This compontent could use some more fancyness. Something like the
    -- log in pop up on angularjs.de would be nice. For now, it will serve,
    -- though.
    let
        signedInStatusComponent =
            case model.auth of
                SignedIn profile ->
                    signedInView profile

                NotSignedIn ->
                    notSignedInView model
    in
        div [ class "signed-in-status" ] signedInStatusComponent


signedInView : Profiles.Types.Profile -> List (Html Msg)
signedInView profile =
    let
        nameComponent =
            span [] [ text profile.name ]

        profileComponents =
            if String.isEmpty profile.gitHubAvatarUrl then
                [ nameComponent ]
            else
                [ img [ src profile.gitHubAvatarUrl ] []
                , nameComponent
                ]

        signOut =
            button [ onClick SignOutClick ] [ text "Abmelden" ]
    in
        profileComponents ++ [ signOut ]


notSignedInView : Model -> List (Html Msg)
notSignedInView model =
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
                [ a
                    [ href gitHubUrl
                    ]
                    [ text "Mit GitHub anmelden" ]
                ]

        otherwise ->
            [ span [] [ text "..." ] ]
