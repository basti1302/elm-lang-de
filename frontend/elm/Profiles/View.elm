module Profiles.View exposing (getProfilePicSrc, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
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
            getProfilePicSrcOrDummy profile 80

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
    div
        [ class "container" ]
        [ div
            [ class "columns" ]
            [ profilePicAndBio
                (class
                    ("column col-sm-12 col-md-6 col-lg-7 col-8 "
                        ++ "profile-pic-and-bio"
                    )
                )
                profile
            , profileDetails
                (class
                    ("column col-sm-12 col-md-6 col-lg-5 col-4 "
                        ++ "table table-striped table-hover profile-details"
                    )
                )
                profile
            ]
        ]


profilePicAndBio : Attribute Msg -> Profile -> Html Msg
profilePicAndBio classes profile =
    let
        profilePicSrc =
            getProfilePicSrcOrDummy profile 200

        profilePic =
            img [ src profilePicSrc, class "profil-pic" ] []

        parts =
            [ profilePic
            , h4 [] [ text profile.name ]
            , biography profile
            ]
    in
        div [ classes ] parts


profileDetails : Attribute Msg -> Profile -> Html Msg
profileDetails classes profile =
    let
        potentialParts =
            [ -- "industry" or "suitcase"?
              profileTextRow "suitcase" profile.job
            , cityAndCountry profile
            , profileLinkRow "link" profile.homepage profile.homepage
            , profileLinkRow
                "github"
                ("https://github.com/" ++ profile.gitHubUsername)
                profile.gitHubUsername
            , profileLinkRow
                "twitter"
                ("https://twitter.com/" ++ profile.twitterHandle)
                profile.twitterHandle
            , availability profile
            , joined profile
            ]

        actualParts =
            List.filterMap identity potentialParts
    in
        table [ classes ] [ tbody [] actualParts ]


profileTextRow : String -> String -> Maybe (Html Msg)
profileTextRow faIconName =
    profileTextRowExtra faIconName []


profileTextRowExtra : String -> List String -> String -> Maybe (Html Msg)
profileTextRowExtra faIconName extraClasses value =
    if String.isEmpty value then
        Nothing
    else
        Just <|
            tr [ String.join " " extraClasses |> class ]
                [ td [] [ span [ faClass faIconName ] [] ]
                , td [] [ text value ]
                ]


profileLinkRow : String -> String -> String -> Maybe (Html Msg)
profileLinkRow faIconName url value =
    let
        hrefAttr =
            href url
    in
        if String.isEmpty value then
            Nothing
        else
            Just <|
                tr []
                    [ td []
                        [ a
                            [ faClass faIconName, hrefAttr ]
                            []
                        ]
                    , td [] [ a [ hrefAttr ] [ text value ] ]
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
        profileTextRow "globe" string


availability : Profile -> Maybe (Html Msg)
availability profile =
    if profile.available then
        profileTextRowExtra "check" [ "green-icon" ] "für Projekte verfügbar"
    else
        Nothing


joined : Profile -> Maybe (Html Msg)
joined profile =
    if not (String.isEmpty profile.createdAt) then
        "Dabei seit "
            ++ profile.createdAt
            |> profileTextRow "calendar-check-o"
    else
        Nothing


faClass : String -> Attribute Msg
faClass faIconName =
    class ("fa fa-lg fa-" ++ faIconName)


getProfilePicSrcOrDummy :
    { profileOrHead | gravatarId : String, gitHubAvatarUrl : String }
    -> Int
    -> String
getProfilePicSrcOrDummy profile gravatarPreferredSize =
    let
        maybePicSrc =
            getProfilePicSrc profile gravatarPreferredSize
    in
        case maybePicSrc of
            Just picSrc ->
                picSrc

            Nothing ->
                "/svgs/elm-logo-mono.svg"


getProfilePicSrc :
    { profileOrHead | gravatarId : String, gitHubAvatarUrl : String }
    -> Int
    -> Maybe String
getProfilePicSrc profile gravatarPreferredSize =
    if not (String.isEmpty profile.gravatarId) then
        Just <|
            "http://www.gravatar.com/avatar/"
                ++ profile.gravatarId
                ++ "?s="
                ++ (toString gravatarPreferredSize)
    else if not (String.isEmpty profile.gitHubAvatarUrl) then
        Just profile.gitHubAvatarUrl
    else
        Nothing


biography : Profile -> Html Msg
biography profile =
    let
        biography =
            if (not (String.isEmpty profile.bio)) then
                profile.bio
            else
                "Für dieses Profil wurde leider noch keine Biographie hinzugefügt  :'‑("
    in
        p [ class "profile-biography" ] [ Markdown.toHtml [] biography ]
