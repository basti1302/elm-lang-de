module OAuth.Server (oAuthServer) where

import           Database.StatementMap
import           OAuth.API             (OAuthAPI)
import           OAuth.GitHub.API      (OAuthGitHubAPI)
import           OAuth.GitHub.Server   (oAuthGitHubServer)
import qualified Util.Config           as Config

import           Database.HDBC         (IConnection)
import           Servant


oAuthServer ::
  IConnection connection =>
  Config.WebConfig
  -> DbConnection connection
  -> Server OAuthAPI
oAuthServer webConfig dbConnection =
  let
    oAuthGitHubAPIHandler :: Server OAuthGitHubAPI
    oAuthGitHubAPIHandler = oAuthGitHubServer webConfig dbConnection
  in
    oAuthGitHubAPIHandler

