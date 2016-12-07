module State exposing (init, update, subscriptions)

import Types exposing (..)
import Homepage.State
import Events.State
import Developers.State
import Navigation exposing (Location, newUrl)
import Site exposing (pageToHash, hashToPage)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            hashToPage location.hash

        ( homepageModel, homepageCmd ) =
            Homepage.State.init

        ( eventsModel, eventsCmd ) =
            Events.State.init

        ( developersModel, developersCmd ) =
            Developers.State.init

        initialModel page =
            { page = page
            , homepage = homepageModel
            , events = eventsModel
            , developers = developersModel
            }

        cmds =
            Cmd.batch
                [ Cmd.map HomepageMsg homepageCmd
                , Cmd.map EventsMsg eventsCmd
                , Cmd.map DevelopersMsg developersCmd
                ]
    in
        ( initialModel page, cmds )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            -- leave the model untouched and issue a command to
            -- change the url, which then triggers ChangePage
            ( model, newUrl <| pageToHash page )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        HomepageMsg homeMsg ->
            let
                ( homepageModel, cmd ) =
                    Homepage.State.update homeMsg model.homepage
            in
                ( { model | homepage = homepageModel }
                , Cmd.map HomepageMsg cmd
                )

        EventsMsg eventMsg ->
            let
                ( eventsModel, cmd ) =
                    Events.State.update eventMsg model.events
            in
                ( { model | events = eventsModel }
                , Cmd.map EventsMsg cmd
                )

        DevelopersMsg developerMsg ->
            let
                ( developersModel, cmd ) =
                    Developers.State.update developerMsg model.developers
            in
                ( { model | developers = developersModel }
                , Cmd.map DevelopersMsg cmd
                )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
