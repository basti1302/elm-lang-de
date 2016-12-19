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

import           Html.API (HtmlAPI)
import           Json.API (JsonAPI)

import           Servant


-- TODO /api/ root document (with links)
-- TODO Provide JSON and HTML at the same URL, where appropriate
-- TODO Write automated tests for HTTP API


type StaticFiles = Raw


type API =
       "api" :> JsonAPI
  :<|> HtmlAPI
  :<|> StaticFiles


proxyApi :: Proxy API
proxyApi = Proxy

