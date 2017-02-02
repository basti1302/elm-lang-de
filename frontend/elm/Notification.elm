module Notification
    exposing
        ( Notification
        , Severity(..)
        , infoWithTimeout
        , infoWithoutTimeout
        , errorWithTimeout
        , errorWithoutTimeout
        , getSeverity
        , getText
        , getTimeout
        )

import Time exposing (Time)


type Severity
    = Error
    | Info


type Notification
    = Notification NotificationRecord


infoWithTimeout : Time -> String -> Notification
infoWithTimeout timeout text =
    Notification
        { severity = Info
        , text = text
        , timeout = Just timeout
        }


infoWithoutTimeout : String -> Notification
infoWithoutTimeout text =
    Notification
        { severity = Info
        , text = text
        , timeout = Nothing
        }


errorWithTimeout : Time -> String -> Notification
errorWithTimeout timeout text =
    Notification
        { severity = Error
        , text = text
        , timeout = Just timeout
        }


errorWithoutTimeout : String -> Notification
errorWithoutTimeout text =
    Notification
        { severity = Error
        , text = text
        , timeout = Nothing
        }


getSeverity : Notification -> Severity
getSeverity (Notification record) =
    record.severity


getText : Notification -> String
getText (Notification record) =
    record.text


getTimeout : Notification -> Maybe Time
getTimeout (Notification record) =
    record.timeout


hasTimeout : Notification -> Bool
hasTimeout (Notification record) =
    case record.timeout of
        Just _ ->
            True

        Nothing ->
            False


type alias NotificationRecord =
    { severity : Severity
    , text : String
    , timeout : Maybe Time
    }
