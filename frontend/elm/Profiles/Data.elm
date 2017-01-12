module Profiles.Data exposing (decodeProfile)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Profiles.Types exposing (Profile)


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
