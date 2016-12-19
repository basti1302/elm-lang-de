module Json.Server (jsonServer) where

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
jsonServer _ dbConnection =
  let
    profilesAPIHandler :: Server ProfileAPI
    profilesAPIHandler = profileServer dbConnection
  in
    profilesAPIHandler

