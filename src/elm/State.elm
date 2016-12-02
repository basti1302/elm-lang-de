module State exposing (init, update, subscriptions)

import Developers.State
import Developers.Types
import Events.State
import Events.Types
import Homepage.State
import Homepage.Types
import Navigation exposing (Location, newUrl)
import Site exposing (hashToPage, pageToHash)
import Types exposing (..)


initialModel : Flags -> Page -> Model
initialModel flags initialPage =
    { page = initialPage
    , homepage = Homepage.State.initialModel
    , events = Events.State.initialModel
    , developers = Developers.State.initialModel flags.profiles
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        initialPage =
            hashToPage location.hash
    in
        ( initialModel flags initialPage, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            -- leave the model untouched and issue a command to
            -- change the url, which then triggers ChangePage
            ( model, newUrl <| pageToHash page )

        ChangePage page ->
            handleChangePage page model

        DevelopersMsg developerMsg ->
            handleDeveloperMsg developerMsg model

        EventsMsg eventMsg ->
            handleEventsMsg eventMsg model

        HomepageMsg homeMsg ->
            handleHomepageMsg homeMsg model


handleChangePage : Page -> Model -> ( Model, Cmd Msg )
handleChangePage page model =
    case page of
        DevelopersPage subPage ->
            let
                subPageChange =
                    Developers.Types.ChangePage subPage
            in
                handleDeveloperMsg subPageChange model

        otherwise ->
            ( { model | page = page }, Cmd.none )


handleDeveloperMsg : Developers.Types.Msg -> Model -> ( Model, Cmd Msg )
handleDeveloperMsg developerMsg model =
    let
        ( developersModel, cmd ) =
            Developers.State.update developerMsg model.developers
    in
        ( { model
            | page = DevelopersPage developersModel.page
            , developers = developersModel
          }
        , Cmd.map DevelopersMsg cmd
        )


handleEventsMsg : Events.Types.Msg -> Model -> ( Model, Cmd Msg )
handleEventsMsg eventMsg model =
    let
        ( eventsModel, cmd ) =
            Events.State.update eventMsg model.events
    in
        ( { model | events = eventsModel }
        , Cmd.map EventsMsg cmd
        )


handleHomepageMsg : Homepage.Types.Msg -> Model -> ( Model, Cmd Msg )
handleHomepageMsg homeMsg model =
    let
        ( homepageModel, cmd ) =
            Homepage.State.update homeMsg model.homepage
    in
        ( { model | homepage = homepageModel }
        , Cmd.map HomepageMsg cmd
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
