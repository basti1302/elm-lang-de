module EditProfile.Translator exposing (Translator, translator)

import EditProfile.Types exposing (..)
import Notification exposing (Notification)


-- See https://medium.com/@alex.lew/the-translator-pattern-a-model-for-child-to-parent-communication-in-elm-f4bfaa1d3f98#.2k0o5utn4
-- for information on the translator pattern to enable child-parent
-- communication in Elm.


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onShowNotification : Notification -> msg
    }


type alias Translator parentMsg =
    Msg -> parentMsg


translator : TranslationDictionary parentMsg -> Translator parentMsg
translator { onInternalMessage, onShowNotification } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal

        ForParent (ShowNotification notification) ->
            onShowNotification notification
