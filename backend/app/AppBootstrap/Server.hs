{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module AppBootstrap.Server (appBootstrapServer) where

import           AppBootstrap.API      (AppBootstrapAPI)
import           AppBootstrap.Response (AppBootstrapResponse (AppBootstrapResponse))
import           AppBootstrap.Response as AppBootstrapResponse
import qualified Util.Config           as Config

import           Servant


appBootstrapServer ::
  Config.WebConfig
  -> Server AppBootstrapAPI
appBootstrapServer webConfig =
  return AppBootstrapResponse
         { AppBootstrapResponse.gitHubClientId = Config.gitHubClientId webConfig
         }

