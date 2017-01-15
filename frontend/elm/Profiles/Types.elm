module Profiles.Types exposing (..)

import RemoteData exposing (WebData)


type Page
    = ListPage
    | DetailsPage {- urlFragment -} String


type alias Model =
    { page : Page
    , profiles : WebData (List ProfileHead)
    , currentProfile : Maybe Profile
    }


{-| A reduced view on a developer profile, intended to be used on the page
listing all developers.
-}
type alias ProfileHead =
    { id : String
    , name : String
    , urlFragment : String
    , job : String
    , city : String
    , gitHubAvatarUrl : String
    , gravatarId : String
    , createdAt : String
    }


type alias Profile =
    { id : String
    , name : String
    , urlFragment : String
    , job : String
    , bio : String
    , available : Bool
    , zipCode : String
    , city : String
    , country : String
    , email : String
    , homepage : String
    , gitHubUsername : String
    , gitHubAvatarUrl : String
    , gravatarId : String
    , twitterHandle : String
    , createdAt : String
    }


type Msg
    = ChangePage Page
    | ProfileDetailsResponse (WebData Profile)
    | ProfileListResponse (WebData (List ProfileHead))
