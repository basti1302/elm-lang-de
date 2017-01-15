module Profiles.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Profiles.Types exposing (..)
import RemoteData exposing (RemoteData(..))


view : Model -> Html Msg
view model =
    case (model.currentProfile) of
        Nothing ->
            listView model

        Just profile ->
            detailsView profile


listView : Model -> Html Msg
listView model =
    case model.profiles of
        Failure _ ->
            div [] [ text "Die List der Entwickler und Entwicklerinnen konnte nicht geladen werden :-(" ]

        Success profiles ->
            listProfiles profiles

        otherwise ->
            div [] [ text "..." ]


listProfiles : List ProfileHead -> Html Msg
listProfiles profiles =
    div [ class "profile-list" ]
        [ ul [] (List.map renderProfileInList profiles)
        ]


renderProfileInList : ProfileHead -> Html Msg
renderProfileInList profile =
    let
        profilePicSrc =
            getProfilePicSrc profile 80

        profilePic =
            [ img [ src profilePicSrc, class "profil-pic" ] [] ]
    in
        li []
            [ a [ href ("#developers/" ++ profile.urlFragment) ]
                (profilePic
                    ++ [ span [] [ text profile.name ]
                       , span [] [ text profile.job ]
                       , span [] [ text profile.city ]
                       ]
                )
            ]


detailsView : Profile -> Html Msg
detailsView profile =
    -- TODO We need way more fancyness here!
    let
        profilePicSrc =
            getProfilePicSrc profile 200

        profilePic =
            img [ src profilePicSrc, class "profil-pic" ] []

        -- TODO Use fontawesome for Twitter, GitHub, job, homepage etc.
        -- TODO Show when user has joined.
        potentialParts =
            [ Just profilePic
            , optionalPartWithoutLabel profile.name
            , optionalPartWithoutLabel profile.job
            , optionalPartWithoutLabel profile.bio
            , cityAndCountry profile
              -- profile.email
            , optionalPartWithLabel "Homepage" profile.homepage
            , optionalPartWithLabel "GitHub" profile.gitHubUsername
            , optionalPartWithLabel "Twitter" profile.twitterHandle
              -- profile.available (Bool)
              -- profile.createdAt
            ]

        actualParts =
            List.filterMap identity potentialParts
    in
        div [ class "profile-details" ] actualParts


optionalPartWithoutLabel : String -> Maybe (Html Msg)
optionalPartWithoutLabel value =
    if String.isEmpty value then
        Nothing
    else
        Just <|
            div [ class "row" ]
                [ span [] [ text value ]
                ]


optionalPartWithLabel : String -> String -> Maybe (Html Msg)
optionalPartWithLabel labelString value =
    if String.isEmpty value then
        Nothing
    else
        Just <|
            div [ class "row" ]
                [ label [ for labelString ] [ text labelString ]
                , span [ id labelString ] [ text value ]
                ]


cityAndCountry : Profile -> Maybe (Html Msg)
cityAndCountry profile =
    let
        string =
            if String.isEmpty profile.city && String.isEmpty profile.country then
                ""
            else if String.isEmpty profile.country then
                profile.city
            else if String.isEmpty profile.city then
                profile.country
            else
                profile.city
                    ++ " ("
                    ++ profile.country
                    ++ ")"
    in
        optionalPartWithoutLabel string


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
