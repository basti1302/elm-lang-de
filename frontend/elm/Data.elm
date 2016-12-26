module Data exposing (loadAppBootstrap)

import Http
import Json.Decode
import Json.Decode.Pipeline
import RemoteData
import Types exposing (..)


loadAppBootstrap : Cmd Msg
loadAppBootstrap =
    Http.get "/api/app-bootstrap" decodeAppBootstrap
        |> RemoteData.sendRequest
        |> Cmd.map AppBootstrapResponse


decodeAppBootstrap : Json.Decode.Decoder AppBootstrapResource
decodeAppBootstrap =
    Json.Decode.Pipeline.decode AppBootstrapResource
        |> Json.Decode.Pipeline.required "gitHubClientId" (Json.Decode.nullable Json.Decode.string)
