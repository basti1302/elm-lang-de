module OAuth.Server (oAuthServer) where

import           OAuth.API           (OAuthAPI)
import           OAuth.GitHub.API    (OAuthGitHubAPI)
import           OAuth.GitHub.Server (oAuthGitHubServer)
import qualified Util.Config         as Config

import           Servant


oAuthServer ::
  Config.WebConfig
  -> Server OAuthAPI
oAuthServer webConfig =
  let
    oAuthGitHubAPIHandler :: Server OAuthGitHubAPI
    oAuthGitHubAPIHandler = oAuthGitHubServer webConfig
  in
    oAuthGitHubAPIHandler

