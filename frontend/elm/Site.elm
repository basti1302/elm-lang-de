module Site exposing (..)

import Navigation exposing (Location)
import Types exposing (..)


locationToMsg : Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage


hashToPage : String -> Page
hashToPage hash =
    case hash of
        "" ->
            HomePage

        "#events" ->
            EventsPage

        "#developers" ->
            ProfilesPage

        _ ->
            NotFound


pageToHash : Page -> String
pageToHash page =
    case page of
        HomePage ->
            "#"

        EventsPage ->
            "#events"

        ProfilesPage ->
            "#developers"

        NotFound ->
            "#notfound"
