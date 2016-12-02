module Developers.State exposing (init, initialModel, update, subscriptions)

import Developers.Site exposing (..)
import Developers.Types exposing (..)
import Maybe.Extra
import Navigation exposing (Location, newUrl)
import Regex


initialModel : List String -> Model
initialModel profiles =
    let
        parseProfile fullMarkdownProfileAsString =
            let
                name =
                    fullMarkdownProfileAsString
                        |> String.split "\n"
                        |> List.head
                        |> Maybe.withDefault "?"

                urlFragment =
                    name
                        |> String.toLower
                        |> Regex.replace Regex.All (Regex.regex "[^\\w]") (\_ -> "")

                profile =
                    fullMarkdownProfileAsString
            in
                { name = name
                , urlFragment = urlFragment
                , profileMarkdown = profile
                }
    in
        { page = DeveloperListPage
        , developers = List.map parseProfile profiles
        , developer = Nothing
        }


init : List String -> ( Model, Cmd Msg )
init profiles =
    ( initialModel profiles, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            -- leave the model untouched and issue a command to
            -- change the url, which then triggers ChangePage
            ( model, newUrl <| pageToHash page )

        ChangePage page ->
            handleChangePage page model

        NoOp ->
            ( model, Cmd.none )


handleChangePage : Page -> Model -> ( Model, Cmd Msg )
handleChangePage page model =
    case page of
        DeveloperDetailsPage urlFragment ->
            let
                developer =
                    findDeveloperByUrlFragment
                        urlFragment
                        model.developers
            in
                if Maybe.Extra.isJust developer then
                    ( { model
                        | page = page
                        , developer = developer
                      }
                    , Cmd.none
                    )
                else
                    -- ignore ChangePage if we can not find the
                    -- referenced developer
                    ( { model | page = DeveloperListPage }, Cmd.none )

        otherwise ->
            ( { model | page = page }, Cmd.none )


findDeveloperByUrlFragment : String -> List Developer -> Maybe Developer
findDeveloperByUrlFragment urlFragment =
    List.filter (\developer -> developer.urlFragment == urlFragment)
        >> List.head


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
