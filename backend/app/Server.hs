{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Server (mainServer) where

import           API                   (API, StaticFiles)
import           Database.StatementMap
import           Json.API              (JsonAPI)
import           Json.Server           (jsonServer)
import           OAuth.API             (OAuthAPI)
import           OAuth.Server          (oAuthServer)
import qualified Util.Config           as Config

import           Database.HDBC         (IConnection)
import           Servant


mainServer ::
  IConnection connection =>
  Config.AppConfig
  -> DbConnection connection
  -> Server API
mainServer appConfig dbConnection =
  let
    webConfig = Config.webConfig appConfig
    jsonAPIHandler :: Server JsonAPI
    jsonAPIHandler = jsonServer appConfig dbConnection
    oAuthAPIHandler :: Server OAuthAPI
    oAuthAPIHandler = oAuthServer webConfig
    staticHandler :: Server StaticFiles
    staticHandler = serveDirectory "frontend"
  in
       jsonAPIHandler
  :<|> oAuthAPIHandler
  :<|> staticHandler

