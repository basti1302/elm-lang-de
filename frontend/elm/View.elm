module View exposing (view)

import EditProfile.View
import Events.View
import GitHubOAuthConfig
import Homepage.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (defaultOptions, onClick)
import Imprint.View
import Json.Decode as Json
import Notification exposing (Notification, Severity(..))
import Profiles.Types
import Profiles.View
import Routes
import Types exposing (..)


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
                                |> Html.map editProfileTranslator

                ImprintPage ->
                    Imprint.View.view

                ProfilesPage _ ->
                    Profiles.View.view model.profiles
                        |> Html.map ProfilesMsg

                NotFoundPage ->
                    notFound
    in
        div
            [ class "overall"
            , onClick CloseAllPopups
            ]
            ([ pageHeader model ]
                ++ notification model
                ++ [ div
                        [ class "main" ]
                        [ pageContent ]
                   , pageFooter
                   ]
            )


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
            navItemAllScreens model.page

        navLinkWideScreen =
            navItemWideScreen model.page

        navLinksSmallScreen =
            authenticationComponentSmallScreen model
    in
        header
            [ class "header" ]
            [ span
                [ class "nav-small-screen-hamburger fa fa-lg fa-bars"
                , onWithStopPropagation ToggleSmallScreenNav
                ]
                []
            , nav
                [ classList
                    [ ( "nav", True )
                    , ( "collapsed", not model.showSmallScreenNav )
                    ]
                ]
                ([ navLinkWideScreen HomePage "Elm" "home"
                 , navLinkSmallScreen model HomePage "Startseite" "home"
                 , navLink EventsPage "Termine" "calendar"
                 , navLink (ProfilesPage Profiles.Types.ListPage) "Entwickler" "group"
                 ]
                    ++ navLinksSmallScreen
                    ++ [ authenticationComponentWideScreen model ]
                )
            ]


navItemAllScreens : Page -> Page -> String -> String -> Html Msg
navItemAllScreens =
    navItem []


navItemSmallScreen : Page -> Page -> String -> String -> Html Msg
navItemSmallScreen =
    navItem [ ( "nav-item-small-screen-only", True ) ]


navItemWideScreen : Page -> Page -> String -> String -> Html Msg
navItemWideScreen =
    navItem [ ( "nav-item-wide-screen-only", True ) ]


navLinkSmallScreen : Model -> Page -> String -> String -> Html Msg
navLinkSmallScreen model =
    navItemSmallScreen model.page


navItem :
    List ( String, Bool )
    -> Page
    -> Page
    -> String
    -> String
    -> Html Msg
navItem extraClasses currentPage page title icon =
    -- TODO Shouldn't navItem produce simple links with the proper #anchor as
    -- href? We might be able to drop the Navigate messages and Routes#pageToHash
    -- completely.
    let
        classes =
            [ ( "nav__item", True )
            , ( "nav__item--current", currentPage == page )
            ]
                ++ extraClasses
    in
        a
            [ (classList classes)
            , onClick (Navigate page)
            ]
            [ span [ class ("fa fa-" ++ icon) ] []
            , text title
            ]


navLinkSignOutSmallScreen : Html Msg
navLinkSignOutSmallScreen =
    a
        [ class "nav__item nav-item-small-screen-only", onClick SignOutClick ]
        [ span [ class "fa fa-sign-out" ] []
        , text "Abmelden"
        ]


notification : Model -> List (Html Msg)
notification model =
    case model.notification of
        Just notification ->
            let
                notificationClass =
                    case Notification.getSeverity notification of
                        Info ->
                            "notification notification-info"

                        Error ->
                            "notification notification-error"
            in
                [ div
                    [ onClick CloseNotification, class notificationClass ]
                    [ span [] [ text <| Notification.getText notification ] ]
                ]

        Nothing ->
            []


pageFooter : Html Msg
pageFooter =
    footer []
        -- small screens: split footer content across two lines
        [ div
            [ class "footer-line hide-wd-sm" ]
            footerContent1
        , div
            [ class "footer-line hide-wd-sm" ]
            footerContent2
          -- wide screens: all footer content in one line
        , div
            [ class "footer-line hide-sm" ]
            (footerContent1 ++ footerContent2)
        ]


footerContent1 : List (Html Msg)
footerContent1 =
    [ text "Made with ♥, "
    , elmLink
    , text " and "
    , servantLink
    , text " by "
    , a [ href "/#developers/basti1302" ]
        [ span [ class "hide-wd-xs" ] [ text "Bastian" ]
        , span [ class "hide-xs" ] [ text "Bastian Krol" ]
        ]
    , text " & "
    , a [ href "/#developers/dennisreimann" ]
        [ span [ class "hide-wd-xs" ] [ text "Dennis" ]
        , span [ class "hide-xs" ] [ text "Dennis Reimann" ]
        ]
    , span [ class "hide-sm" ] [ text " | " ]
    ]


footerContent2 : List (Html Msg)
footerContent2 =
    [ a [ href "#imprint" ] [ text "Impressum" ]
    , text " | Sponsored by "
    , ccLink
    ]


elmLink : Html Msg
elmLink =
    a [ href "http://elm-lang.org" ]
        [ img [ src "/img/elm-logo-mono.svg" ] []
        , text "Elm"
        ]


servantLink : Html Msg
servantLink =
    a [ href "https://haskell-servant.readthedocs.io" ] [ text "Servant" ]


ccLink : Html Msg
ccLink =
    a
        [ style [ ( "position", "relative" ), ( "top", "3px" ) ]
        , href "https://www.codecentric.de"
        ]
        [ img [ src "/img/cc-logo-black.png" ] [] ]


authenticationComponentWideScreen : Model -> Html Msg
authenticationComponentWideScreen model =
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

        maybeProfilePicSrc =
            Profiles.View.getProfilePicSrc profile 40

        mainProfileComponent =
            case maybeProfilePicSrc of
                Just profilePicSrc ->
                    span []
                        [ img [ src profilePicSrc, class "header-profile-pic" ] []
                        , span [ class "fa fa-chevron-down" ] []
                        ]

                Nothing ->
                    span []
                        [ text profile.name
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
    in
        -- stopPropagation is required so the popup is not closed immediately
        -- because the click propagates to the main div which always triggers a
        -- CloseAllPopups event.
        div
            [ class "signed-in-status"
            , onWithStopPropagation ToggleProfilePopupMenu
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
        maybeUrl =
            GitHubOAuthConfig.gitHubOAuthUrl model Nothing

        comp =
            case maybeUrl of
                Just ghUrl ->
                    a
                        [ href ghUrl
                        , class "btn login-button"
                        ]
                        [ span [ class "fa fa-github" ] []
                        , text "Anmelden"
                        ]

                Nothing ->
                    img [ src "img/loading.gif", class "auth-waiting" ] []
    in
        div
            [ class "signed-in-status" ]
            [ comp ]


authenticationComponentSmallScreen : Model -> List (Html Msg)
authenticationComponentSmallScreen model =
    case model.auth of
        SignedIn _ ->
            [ navLinkSmallScreen model EditProfilePage "Mein Profil" "user"
            , navLinkSignOutSmallScreen
            ]

        NotSignedIn ->
            signInWithGitHubSmallScreen model


signInWithGitHubSmallScreen : Model -> List (Html Msg)
signInWithGitHubSmallScreen model =
    let
        maybeUrl =
            Routes.pageToHash model.page
                |> Just
                |> GitHubOAuthConfig.gitHubOAuthUrl model
    in
        case maybeUrl of
            Just ghUrl ->
                [ a [ href ghUrl, class "nav__item nav-item-small-screen-only" ]
                    [ span [ class "fa fa-sign-out" ] []
                    , text "Anmelden"
                    ]
                ]

            Nothing ->
                []


onWithStopPropagation : msg -> Attribute msg
onWithStopPropagation msg =
    Html.Events.onWithOptions
        "click"
        ({ defaultOptions | stopPropagation = True })
        (Json.succeed msg)
