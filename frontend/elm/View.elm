module View exposing (view)

import Types exposing (..)
import Html exposing (..)
import Html.Events exposing (defaultOptions, onClick)
import Html.Attributes exposing (..)
import Homepage.View
import EditProfile.View
import Events.View
import Json.Decode as Json
import Profiles.View
import Profiles.Types
import RemoteData exposing (RemoteData(..))


view : Model -> Html Msg
view model =
    let
        pageContent =
            case model.page of
                HomePage ->
                    Homepage.View.view model.homepage
                        |> Html.map HomepageMsg

                EventsPage ->
                    Events.View.view model.events
                        |> Html.map EventsMsg

                EditProfilePage ->
                    case model.auth of
                        NotSignedIn ->
                            blankPage

                        SignedIn signedInModel ->
                            EditProfile.View.view
                                signedInModel.editProfileModel
                                |> Html.map EditProfileMsg

                ProfilesPage _ ->
                    Profiles.View.view model.profiles
                        |> Html.map ProfilesMsg

                NotFound ->
                    notFound
    in
        div
            [ class "overall"
            , onClick CloseAllPopups
            ]
            [ pageHeader model
            , div
                [ class "main" ]
                [ pageContent ]
            ]


notFound : Html Msg
notFound =
    div [] [ text "404" ]


blankPage : Html Msg
blankPage =
    div [] []



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
    let
        signedInStatusComponent =
            case model.auth of
                SignedIn signedInModel ->
                    signedInComponent model signedInModel

                NotSignedIn ->
                    notSignedInComponent model
    in
        signedInStatusComponent


signedInComponent : Model -> SignedInModel -> Html Msg
signedInComponent model signedInModel =
    let
        profile =
            signedInModel.profile

        mainProfileComponent =
            if (String.isEmpty profile.gitHubAvatarUrl) then
                span []
                    [ text profile.name
                    , span [ class "fa fa-chevron-down" ] []
                    ]
            else
                -- TODO Also show gravatar image if no githubavatarurl is set!
                --      See Profiles.View#getProfilePicSrc
                span []
                    [ img [ src profile.gitHubAvatarUrl ] []
                    , span [ class "fa fa-chevron-down" ] []
                    ]

        profilePopupMenu =
            profilePopupMenuComponent model signedInModel

        profileComponentList =
            case profilePopupMenu of
                Just menu ->
                    [ mainProfileComponent, menu ]

                Nothing ->
                    [ mainProfileComponent ]

        -- stopPropagation is required so the popup is not closed immediately
        -- because the click propagates to the main div which always triggers a
        -- CloseAllPopups event.
        onClickToggleProfilePopupMenu =
            Html.Events.onWithOptions
                "click"
                ({ defaultOptions | stopPropagation = True })
                (Json.succeed ToggleProfilePopupMenu)
    in
        div
            [ class "signed-in-status"
            , onClickToggleProfilePopupMenu
            ]
            profileComponentList


profilePopupMenuComponent : Model -> SignedInModel -> Maybe (Html Msg)
profilePopupMenuComponent model signedInModel =
    if signedInModel.showProfilePopupMenu then
        let
            editMyProfile =
                a [ href "#editprofile" ]
                    [ span [ class "fa fa-user" ] []
                    , span [] [ text "Mein Profil" ]
                    ]

            signOut =
                a [ onClick SignOutClick, class "sign-out" ]
                    [ span [ class "fa fa-sign-out" ] []
                    , span [] [ text "Abmelden" ]
                    ]
        in
            div [ class "profile-popup-menu" ] [ editMyProfile, signOut ]
                |> Just
    else
        Nothing


notSignedInComponent : Model -> Html Msg
notSignedInComponent model =
    let
        comp =
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
                            [ href gitHubUrl
                            , class "btn login-button"
                            ]
                            [ span [ class "fa fa-github" ] []
                            , text "Anmelden"
                            ]

                otherwise ->
                    span [] [ text "..." ]
    in
        div
            [ class "signed-in-status" ]
            [ comp ]
