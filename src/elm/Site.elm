module Site exposing (hashToPage, locationToMsg, pageToHash)

import Developers.Site
import Navigation exposing (Location)
import Types exposing (..)


locationToMsg : Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage


hashToPage : String -> Page
hashToPage hash =
    if String.startsWith "#developers" hash then
        DevelopersPage <| Developers.Site.hashToPage hash
    else
        case hash of
            "" ->
                HomePage

            "#events" ->
                EventsPage

            _ ->
                NotFound


pageToHash : Page -> String
pageToHash page =
    case page of
        HomePage ->
            "#"

        EventsPage ->
            "#events"

        DevelopersPage subPage ->
            Developers.Site.pageToHash subPage

        NotFound ->
            "#notfound"
