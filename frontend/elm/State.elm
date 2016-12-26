module State exposing (init, update, subscriptions)

import Data exposing (loadAppBootstrap)
import Developers.State
import Events.State
import Homepage.State
import Navigation exposing (Location, newUrl)
import RemoteData exposing (RemoteData(..))
import Site exposing (pageToHash, hashToPage)
import Types exposing (..)


initialModel : Page -> Model
initialModel page =
    { page = page
    , homepage = Homepage.State.initialModel
    , events = Events.State.initialModel
    , developers = Developers.State.initialModel
    , gitHubClientId = Nothing
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            hashToPage location.hash
    in
        ( initialModel page, loadAppBootstrap )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            -- leave the model untouched and issue a command to
            -- change the url, which then triggers ChangePage
            ( model, newUrl <| pageToHash page )

        AppBootstrapResponse (Success appBootstrapResource) ->
            ( { model
                | gitHubClientId = appBootstrapResource.gitHubClientId
              }
            , Cmd.none
            )

        AppBootstrapResponse _ ->
            -- Ignore all other app bootstrap responses for now
            ( model, Cmd.none )

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
