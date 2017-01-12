module Profiles.Types exposing (..)


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


type alias Model =
    { developers : List Profile }


type Msg
    = NoOp
