module EditProfile.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import EditProfile.Types exposing (..)


view : Model -> Html Msg
view model =
    -- TODO We need way more fancyness here!
    let
        profile =
            model.profile

        profilePicSrc =
            getProfilePicSrc profile 200

        profilePic =
            img [ src profilePicSrc, class "profil-pic" ] []

        -- TODO Use fontawesome for Twitter, GitHub, job, homepage etc.
        -- TODO Show date when user has joined.
        parts =
            [ profilePic
            , textInputWithLabel "Name" profile.name Name
            , textInputWithLabel "Was machst du beruflich?" profile.job Job
            , textAreaWithHeadline "Über mich" profile.bio Bio
            , textInputWithLabel "Stadt" profile.city City
            , textInputWithLabel "Land" profile.country Country
            , textInputWithLabel "E-Mail (nicht öffentlich sichtbar)" profile.email EMail
            , textInputWithLabel "Homepage" profile.homepage HomePage
            , textInputWithLabel "GitHub" profile.gitHubUsername GitHubUsername
            , textInputWithLabel "Twitter" profile.twitterHandle TwitterHandle
              -- , profile.available (Bool)
              -- , profile.createdAt
            , button [ onClick UpdateProfile ] [ text "Speichern" ]
            ]
    in
        div [ class "profile-details" ] parts


textInputWithLabel : String -> String -> (String -> Msg) -> Html Msg
textInputWithLabel labelString val msg =
    div []
        [ label [ for labelString ] [ text labelString ]
        , input
            [ id labelString
            , name labelString
            , type_ "text"
            , value val
            , onInput msg
            ]
            []
        ]


textAreaWithHeadline : String -> String -> (String -> Msg) -> Html Msg
textAreaWithHeadline labelString val msg =
    div []
        [ label [ for labelString ] [ text labelString ]
        , br [] []
        , text "(Markdown kann zur Formatierung verwendet werden.)"
        , br [] []
        , textarea
            [ id labelString
            , name labelString
            , value val
            , onInput msg
            ]
            []
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
