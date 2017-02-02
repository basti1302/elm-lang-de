module Util.CmdHelper
    exposing
        ( delay
        , msgToCmd
        )

import Process
import Task
import Time exposing (Time)


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.succeed msg
        |> Task.perform identity


delay : Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity
