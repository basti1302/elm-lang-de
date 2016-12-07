module Events.Data exposing (loadContent)

import Http
import Markdown
import RemoteData
import Html exposing (Html)
import Events.Types exposing (..)


loadContent : Cmd Msg
loadContent =
    Http.send (ContentResponse << RemoteData.map transformMarkdown << RemoteData.fromResult) <|
        Http.getString "/content/events.md"


transformMarkdown : String -> Html Msg
transformMarkdown md =
    Markdown.toHtml [] md
