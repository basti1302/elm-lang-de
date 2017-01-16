module Routes exposing (..)

import Navigation exposing (Location)
import Profiles.Routes
import Types exposing (Msg(ChangePage), Page(..))


locationToMsg : Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage


hashToPage : String -> Page
hashToPage hash =
    if String.isEmpty hash then
        HomePage
    else if String.startsWith "#events" hash then
        EventsPage
    else if String.startsWith "#developers" hash then
        String.dropLeft (String.length "#developers") hash
            |> Profiles.Routes.hashToPage
            |> ProfilesPage
    else if String.startsWith "#editprofile" hash then
        EditProfilePage
    else
        NotFound


pageToHash : Page -> String
pageToHash page =
    case page of
        HomePage ->
            "#"

        EventsPage ->
            "#events"

        EditProfilePage ->
            "#editprofile"

        ProfilesPage _ ->
            "#developers"

        NotFound ->
            "#notfound"
