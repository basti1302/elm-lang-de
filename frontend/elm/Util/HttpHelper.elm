module Util.HttpHelper
    exposing
        ( noContentRequest
        , put
        )

import Http
import Json.Decode as Decode
import Json.Encode as Encode


put :
    String
    -> Encode.Value
    -> Decode.Decoder responseType
    -> Http.Request responseType
put url body responseDecoder =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson responseDecoder
        , timeout = Nothing
        , withCredentials = False
        }


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
