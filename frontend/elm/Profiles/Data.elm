module Profiles.Data
    exposing
        ( decodeProfile
        , encodeProfile
        , loadProfileList
        , loadProfileDetails
        )

import Http
import Json.Decode as Decode
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Profiles.Types exposing (..)
import Regex exposing (HowMany(All))
import RemoteData


loadProfileList : Cmd Msg
loadProfileList =
    -- TODO Server side pagination!
    Http.get "/api/profiles" decodeProfiles
        |> RemoteData.sendRequest
        |> Cmd.map ProfileListResponse


decodeProfiles : Decoder (List ProfileHead)
decodeProfiles =
    Decode.list decodeProfileHead


decodeProfileHead : Decoder ProfileHead
decodeProfileHead =
    Pipeline.decode ProfileHead
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional "urlFragment" Decode.string ""
        |> Pipeline.optional "job" Decode.string ""
        |> Pipeline.optional "city" Decode.string ""
        |> Pipeline.optional "gitHubAvatarUrl" Decode.string ""
        |> Pipeline.optional "gravatarId" Decode.string ""
        |> Pipeline.optional "createdAt" Decode.string ""


loadProfileDetails : String -> Cmd Msg
loadProfileDetails urlFragment =
    Http.get ("/api/profiles/" ++ urlFragment) decodeProfile
        |> RemoteData.sendRequest
        |> Cmd.map ProfileDetailsResponse


decodeProfile : Decoder Profile
decodeProfile =
    Pipeline.decode Profile
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional "urlFragment" Decode.string ""
        |> Pipeline.optional "job" Decode.string ""
        |> Pipeline.optional "bio" Decode.string ""
        |> Pipeline.optional "available" Decode.bool False
        |> Pipeline.optional "zipCode" Decode.string ""
        |> Pipeline.optional "city" Decode.string ""
        |> Pipeline.optional "country" Decode.string ""
        |> Pipeline.optional "email" Decode.string ""
        |> Pipeline.optional "homepage" Decode.string ""
        |> Pipeline.optional "gitHubUsername" Decode.string ""
        |> Pipeline.optional "gitHubAvatarUrl" Decode.string ""
        |> Pipeline.optional "gravatarId" Decode.string ""
        |> Pipeline.optional "twitterHandle" Decode.string ""
        |> Pipeline.optional "createdAt" decodeDate ""


decodeDate : Decoder String
decodeDate =
    Decode.map convertDate Decode.string


convertDate : String -> String
convertDate =
    let
        formatDate { submatches } =
            case submatches of
                (Just year) :: ((Just month) :: ((Just day) :: _)) ->
                    String.join "." [ day, month, year ]

                otherwise ->
                    ""
    in
        Regex.replace
            All
            (Regex.regex "(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d).*")
            formatDate


encodeProfile : Profile -> Encode.Value
encodeProfile record =
    Encode.object
        [ ( "name", Encode.string <| record.name )
        , ( "urlFragment", Encode.string <| record.urlFragment )
        , ( "job", Encode.string <| record.job )
        , ( "bio", Encode.string <| record.bio )
        , ( "available", Encode.bool <| record.available )
        , ( "zipCode", Encode.string <| record.zipCode )
        , ( "city", Encode.string <| record.city )
        , ( "country", Encode.string <| record.country )
        , ( "email", Encode.string <| record.email )
        , ( "homepage", Encode.string <| record.homepage )
        , ( "gitHubUsername", Encode.string <| record.gitHubUsername )
        , ( "gravatarId", Encode.string <| record.gravatarId )
        , ( "twitterHandle", Encode.string <| record.twitterHandle )
        ]
