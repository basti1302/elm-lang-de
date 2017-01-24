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
    else if String.startsWith "#imprint" hash then
        ImprintPage
    else if String.startsWith "#developers" hash then
        String.dropLeft (String.length "#developers") hash
            |> Profiles.Routes.hashToPage
            |> ProfilesPage
    else if String.startsWith "#editprofile" hash then
        EditProfilePage
    else
        NotFoundPage


pageToHash : Page -> String
pageToHash page =
    case page of
        HomePage ->
            "#"

        EventsPage ->
            "#events"

        EditProfilePage ->
            "#editprofile"

        ImprintPage ->
            "#imprint"

        ProfilesPage _ ->
            "#developers"

        NotFoundPage ->
            "#notfound"
