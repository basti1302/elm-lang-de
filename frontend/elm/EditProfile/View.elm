module EditProfile.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import EditProfile.Types exposing (..)
import Profiles.Types exposing (Profile)


view : Model -> Html Msg
view model =
    let
        profile =
            model.profile

        profilePicSrc =
            getProfilePicSrc profile 200

        profilePic =
            img [ src profilePicSrc, class "profil-pic" ] []

        parts =
            [ profilePic
            , textInputWithLabel "name" "Name" profile.name Name
              -- TODO Editable url-fragment
            , textInputWithLabel "job" "Was machst du beruflich?" profile.job Job
            , biographyComponent model
            , textInputWithLabel "city" "Stadt" profile.city City
            , textInputWithLabel "country" "Land" profile.country Country
            , textInputWithLabel "email" "E-Mail (nicht öffentlich sichtbar)" profile.email EMail
            , textInputWithLabel "homepage" "Homepage" profile.homepage HomePage
            , github profile
            , twitter profile
            , available profile
            , joined profile
            , div [ class "form-group" ]
                [ button
                    [ class "btn btn-primary btn-lg btn-block"
                    , onClick UpdateProfile
                    ]
                    [ text "Speichern" ]
                ]
            ]
    in
        div [ class "edit-profile-form" ] parts


biographyComponent : Model -> Html Msg
biographyComponent model =
    let
        profile =
            model.profile

        fieldId =
            "bio"

        labelText =
            "Über mich (Markdown)"

        msg =
            Bio

        val =
            profile.bio

        editView =
            textarea
                [ id fieldId
                , class "form-input"
                , name fieldId
                , onInput msg
                , placeholder labelText
                , rows 10
                , value val
                ]
                []

        preview =
            Markdown.toHtml [ class "biography-preview" ] val

        editOrPreview =
            if model.showBiographyPreview then
                preview
            else
                editView

        tabAttribs m isPreview =
            let
                event =
                    if isPreview then
                        SwitchToBiographyPreview
                    else
                        SwitchToBiographyEdit

                attribs =
                    [ onClick event ]
            in
                if m.showBiographyPreview == isPreview then
                    [ class "active" ] ++ attribs
                else
                    attribs

        tabs =
            ul [ class "tab tab-block" ]
                [ li
                    [ class "tab-item" ]
                    [ a
                        (tabAttribs model False)
                        [ span [ class "biography-preview-icon fa fa-pencil" ] []
                        , text "Bearbeiten"
                        ]
                    ]
                , li
                    [ class "tab-item" ]
                    [ a
                        (tabAttribs model True)
                        [ span [ class "biography-preview-icon fa fa-eye" ] []
                        , text "Vorschau"
                        ]
                    ]
                ]
    in
        div
            [ class "form-group" ]
            [ label [ class "form-label", for fieldId ] [ text labelText ]
            , tabs
            , editOrPreview
            ]


github : Profile -> Html Msg
github profile =
    textInputGroup
        "github"
        "GitHub"
        "github.com/"
        profile.gitHubUsername
        GitHubUsername


twitter : Profile -> Html Msg
twitter profile =
    textInputGroup
        "twitter"
        "Twitter"
        "@"
        profile.twitterHandle
        TwitterHandle


available : Profile -> Html Msg
available profile =
    div
        [ class "form-group" ]
        [ label
            [ class "form-switch" ]
            [ input
                [ id "available"
                , checked profile.available
                , name "available"
                , onCheck Available
                , type_ "checkbox"
                ]
                []
            , span [ class "form-icon" ] []
            , text "Verfügbar für Projekte"
            ]
        ]


joined : Profile -> Html Msg
joined profile =
    div [ class "form-group" ]
        [ label [ class "form-label", for "joined" ] [ text "Dabei seit" ]
        , input
            [ id "joined"
            , class "form-input"
            , disabled True
            , name "joined"
            , readonly True
            , type_ "text"
            , value profile.createdAt
            ]
            []
        ]


textInputWithLabel : String -> String -> String -> (String -> Msg) -> Html Msg
textInputWithLabel fieldId labelText val msg =
    div [ class "form-group" ]
        [ label [ class "form-label", for fieldId ] [ text labelText ]
        , input
            [ id fieldId
            , class "form-input"
            , name fieldId
            , onInput msg
            , placeholder labelText
            , type_ "text"
            , value val
            ]
            []
        ]


textInputGroup : String -> String -> String -> String -> (String -> Msg) -> Html Msg
textInputGroup fieldId labelText addonText val msg =
    div [ class "form-group" ]
        [ label [ class "form-label", for fieldId ] [ text labelText ]
        , div [ class "input-group" ]
            [ span
                [ class "input-group-addon" ]
                [ text addonText ]
            , input
                [ id fieldId
                , class "form-input"
                , name fieldId
                , onInput msg
                , placeholder labelText
                , type_ "text"
                , value val
                ]
                []
            ]
        ]


getProfilePicSrc :
    { profileOrHead | gravatarId : String, gitHubAvatarUrl : String }
    -> Int
    -> String
getProfilePicSrc profile gravatarPreferredSize =
    if not (String.isEmpty profile.gravatarId) then
        "http://www.gravatar.com/avatar/"
            ++ profile.gravatarId
            ++ "?s="
            ++ (toString gravatarPreferredSize)
    else if not (String.isEmpty profile.gitHubAvatarUrl) then
        profile.gitHubAvatarUrl
    else
        "/svgs/elm-logo-mono.svg"
