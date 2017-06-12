module Json.Server (jsonServer) where

import           AppBootstrap.API      (AppBootstrapAPI)
import           AppBootstrap.Server   (appBootstrapServer)
import           Database.StatementMap
import           Event.API             (EventAPI)
import           Event.Server          (eventServer)
import           Json.API              (JsonAPI)
import           Profile.API           (ProfileAPI)
import           Profile.Server        (profileServer)
import           SignOut.API           (SignOutAPI)
import           SignOut.Server        (signOutServer)
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
    appBootstrapAPIHandler = appBootstrapServer webConfig dbConnection
    profilesAPIHandler :: Server ProfileAPI
    profilesAPIHandler = profileServer dbConnection
    eventsAPIHandler :: Server EventAPI
    eventsAPIHandler = eventServer dbConnection
    signOutAPIHandler :: Server SignOutAPI
    signOutAPIHandler = signOutServer webConfig
  in
         appBootstrapAPIHandler
    :<|> profilesAPIHandler
    :<|> eventsAPIHandler
    :<|> signOutAPIHandler

