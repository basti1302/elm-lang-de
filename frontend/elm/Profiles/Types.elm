module Profiles.Types exposing (..)

import RemoteData exposing (WebData)


type alias Model =
    { profiles : WebData (List ProfileHead) }


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
    = NoOp
    | ProfilesResponse (WebData (List ProfileHead))
