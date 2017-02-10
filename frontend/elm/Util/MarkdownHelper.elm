module Util.MarkdownHelper exposing (markdownToHtmlSafe)

import Html exposing (..)
import Markdown


safeMarkdownOptions : Markdown.Options
safeMarkdownOptions =
    let
        defaults =
            Markdown.defaultOptions
    in
        { defaults | sanitize = True }


markdownToHtmlSafe : List (Attribute a) -> String -> Html a
markdownToHtmlSafe attributes userInput =
    Markdown.toHtmlWith safeMarkdownOptions attributes userInput
