module Data exposing (loadAppBootstrap, signOut)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Profiles.Data exposing (decodeProfile)
import RemoteData
import Types exposing (..)


loadAppBootstrap : Cmd Msg
loadAppBootstrap =
    Http.get "/api/app-bootstrap" decodeAppBootstrap
        |> RemoteData.sendRequest
        |> Cmd.map AppBootstrapResponse


decodeAppBootstrap : Decoder AppBootstrapResource
decodeAppBootstrap =
    decode AppBootstrapResource
        |> required "signedIn" bool
        |> optional "profile" (nullable decodeProfile) Nothing
        |> required "gitHubClientId" (nullable string)
        |> required "gitHubOAuthRedirectUrl" (string)


signOut : Cmd Msg
signOut =
    noContentRequest "/api/sign-out" "POST"
        |> Http.send SignOutResponse


noContentRequest :
    String
    -> String
    -> Http.Request ()
noContentRequest url verb =
    Http.request
        { method = verb
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
