module Util.HttpHelper
    exposing
        ( decodeMessages
        , HasMessages
        , noContentRequest
        , put
        )

import Http
import Json.Decode as Decode
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Decode.Pipeline as Pipeline


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


{-| A generic type alias for JSON responses that have a messages field for
validation messages.
-}
type alias HasMessages =
    { messages : List String
    }


decodeMessages : Decoder HasMessages
decodeMessages =
    Pipeline.decode HasMessages
        |> Pipeline.required "messages" (Decode.list Decode.string)
