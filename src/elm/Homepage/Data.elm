module Homepage.Data exposing (loadContent)

import Http
import Markdown
import RemoteData
import Html exposing (Html)
import Homepage.Types exposing (..)


loadContent : Cmd Msg
loadContent =
    Http.send (ContentResponse << RemoteData.map transformMarkdown << RemoteData.fromResult) <|
        Http.getString "/content/homepage.md"


transformMarkdown : String -> Html Msg
transformMarkdown md =
    Markdown.toHtml [] md
