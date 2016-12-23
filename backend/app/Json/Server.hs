module Json.Server (jsonServer) where

import           AppBootstrap.API      (AppBootstrapAPI)
import           AppBootstrap.Server   (appBootstrapServer)
import           Database.StatementMap
import           Json.API              (JsonAPI)
import           Profile.API           (ProfileAPI)
import           Profile.Server        (profileServer)
import qualified Util.Config           as Config

import           Database.HDBC         (IConnection)
import           Servant


jsonServer ::
  IConnection connection =>
  Config.AppConfig
  -> DbConnection connection
  -> Server JsonAPI
jsonServer appConfig dbConnection =
  let
    webConfig = Config.webConfig appConfig
    appBootstrapAPIHandler :: Server AppBootstrapAPI
    appBootstrapAPIHandler = appBootstrapServer webConfig
    profilesAPIHandler     :: Server ProfileAPI
    profilesAPIHandler     = profileServer dbConnection
  in
         appBootstrapAPIHandler
    :<|> profilesAPIHandler

