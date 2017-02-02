module EditProfile.Data exposing (updateProfile)

import RemoteData
import EditProfile.Types exposing (..)
import Profiles.Data exposing (decodeProfile, encodeProfile)
import Profiles.Types exposing (Profile)
import Util.HttpHelper as HttpHelper


noOp : a -> a
noOp =
    identity


updateProfile : Profile -> Cmd Msg
updateProfile profile =
    let
        body =
            encodeProfile profile
    in
        HttpHelper.put ("/api/profiles/" ++ profile.id) body decodeProfile
            |> RemoteData.sendRequest
            |> Cmd.map UpdateProfileResponse
            |> Cmd.map ForSelf
