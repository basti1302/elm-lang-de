module Developers.Site exposing (developerToHash, hashToPage, pageToHash)

-- Maybe Developers.Routes or Developers.Routing or some such would be a better
-- name for these kind of modules? Site only makes sense for the root routing
-- module I think, OTOH, all modules that contain hashToPage/pageToHash
-- functions should preferably have the same name.

import Developers.Types exposing (..)


hashToPage : String -> Page
hashToPage hash =
    if hash == "#developers" then
        DeveloperListPage
    else if hash == "#developers?developer=dennisreimann" then
        DeveloperDetailsPage "dennisreimann"
    else if hash == "#developers?developer=bastiankrol" then
        DeveloperDetailsPage "bastiankrol"
    else
        DeveloperListPage


pageToHash : Page -> String
pageToHash page =
    case page of
        DeveloperListPage ->
            "#developers"

        DeveloperDetailsPage urlFragment ->
            developerUrl urlFragment


developerToHash : Developer -> String
developerToHash developer =
    developerUrl developer.urlFragment


developerUrl : String -> String
developerUrl urlFragment =
    "#developers?developer=" ++ urlFragment
