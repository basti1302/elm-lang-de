{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : API
Description : The main API of elm-lang-de

Servants distinctive feature is to describe HTTP APIs as types. This module
is the root of the elm-lang-de API type. All other APIs are sub-APIs of the root
API.

-}
module API (API, proxyApi, StaticFiles) where

import           Json.API  (JsonAPI)
import           OAuth.API (OAuthAPI)

import           Servant


type StaticFiles = Raw


type API =
       "api"   :> JsonAPI
  :<|> "oauth" :> OAuthAPI
  :<|> StaticFiles


proxyApi :: Proxy API
proxyApi = Proxy

