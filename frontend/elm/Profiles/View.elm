module Profiles.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Profiles.Types exposing (..)
import RemoteData exposing (RemoteData(..))


view : Model -> Html Msg
view model =
    case model.profiles of
        Failure _ ->
            div [] [ text "Die List der Entwickler und Entwicklerinnen konnte nicht geladen werden :-(" ]

        Success profiles ->
            listProfiles profiles

        otherwise ->
            div [] [ text "..." ]


listProfiles : List ProfileHead -> Html Msg
listProfiles profiles =
    div [ class "profiles" ]
        [ ul [] (List.map renderOneProfile profiles)
        ]


renderOneProfile : ProfileHead -> Html Msg
renderOneProfile profile =
    let
        profilePicSrc =
            if not (String.isEmpty profile.gravatarId) then
                "http://www.gravatar.com/avatar/"
                    ++ profile.gravatarId
                    ++ "?s=80"
            else if not (String.isEmpty profile.gitHubAvatarUrl) then
                profile.gitHubAvatarUrl
            else
                "/svgs/elm-logo-mono.svg"

        profilePic =
            [ img [ src profilePicSrc, class "profil-pic" ] [] ]
    in
        li [{- onClick GoToProfile profile.urlFragment -}]
            (profilePic
                ++ [ span
                        [ class "profile-text profile-name" ]
                        [ text profile.name ]
                   , span
                        [ class "profile-text profile-job" ]
                        [ text profile.job ]
                   , span
                        [ class "profile-text profile-city" ]
                        [ text profile.city ]
                   ]
            )
