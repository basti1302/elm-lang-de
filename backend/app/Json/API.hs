{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Json.API (JsonAPI) where

import           Profile.API (ProfileAPI)

import           Servant


type JsonAPI =
       "profiles"
       :> ProfileAPI

