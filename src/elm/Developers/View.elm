module Developers.View exposing (view)

import Developers.Site exposing (developerToHash)
import Developers.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown


view : Model -> Html Msg
view model =
    case ( model.page, model.developer ) of
        ( DeveloperDetailsPage _, Just developer ) ->
            developerDetails developer

        otherwise ->
            developerList model



-- LAYOUT (private)


developerList : Model -> Html Msg
developerList model =
    ul [] (List.map profileLink model.developers)


profileLink : Developer -> Html Msg
profileLink developer =
    li []
        [ a
            [ href <| developerToHash developer ]
            [ text developer.name ]
        ]


developerDetails : Developer -> Html Msg
developerDetails developer =
    Markdown.toHtmlWith markdownOptions [] developer.profileMarkdown


markdownOptions : Markdown.Options
markdownOptions =
    let
        opts =
            Markdown.defaultOptions
    in
        { opts
            | githubFlavored = Just { tables = True, breaks = False }
            , sanitize = True
            , smartypants = True
        }
