module Profiles.Data
    exposing
        ( decodeProfile
        , loadProfileList
        , loadProfileDetails
        )

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Profiles.Types exposing (..)
import RemoteData


loadProfileList : Cmd Msg
loadProfileList =
    -- TODO Server side pagination!
    Http.get "/api/profiles" decodeProfiles
        |> RemoteData.sendRequest
        |> Cmd.map ProfileListResponse


decodeProfiles : Decoder (List ProfileHead)
decodeProfiles =
    list decodeProfileHead


decodeProfileHead : Decoder ProfileHead
decodeProfileHead =
    decode ProfileHead
        |> required "id" string
        |> required "name" string
        |> optional "urlFragment" string ""
        |> optional "job" string ""
        |> optional "city" string ""
        |> optional "gitHubAvatarUrl" string ""
        |> optional "gravatarId" string ""
        |> optional "createdAt" string ""


loadProfileDetails : String -> Cmd Msg
loadProfileDetails urlFragment =
    Http.get ("/api/profiles/" ++ urlFragment) decodeProfile
        |> RemoteData.sendRequest
        |> Cmd.map ProfileDetailsResponse


decodeProfile : Decoder Profile
decodeProfile =
    decode Profile
        |> required "id" string
        |> required "name" string
        |> optional "urlFragment" string ""
        |> optional "job" string ""
        |> optional "bio" string ""
        |> optional "available" bool False
        |> optional "zipCode" string ""
        |> optional "city" string ""
        |> optional "country" string ""
        |> optional "email" string ""
        |> optional "homepage" string ""
        |> optional "gitHubUsername" string ""
        |> optional "gitHubAvatarUrl" string ""
        |> optional "gravatarId" string ""
        |> optional "twitterHandle" string ""
        |> optional "createdAt" string ""
