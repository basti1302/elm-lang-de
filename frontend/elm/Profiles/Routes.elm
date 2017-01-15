module Profiles.Routes exposing (..)

import Navigation exposing (Location)
import Profiles.Types exposing (..)


locationToMsg : Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage


hashToPage : String -> Page
hashToPage hash =
    if String.isEmpty hash then
        ListPage
    else
        let
            urlFragment =
                if String.startsWith "/" hash then
                    String.dropLeft 1 hash
                else
                    hash
        in
            DetailsPage urlFragment


pageToHash : Page -> String
pageToHash page =
    case page of
        ListPage ->
            ""

        DetailsPage urlFragment ->
            "/" ++ urlFragment
