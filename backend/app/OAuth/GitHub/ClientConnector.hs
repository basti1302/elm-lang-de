module OAuth.GitHub.ClientConnector where

import qualified OAuth.GitHub.ClientAPI   as ClientAPI
import           OAuth.GitHub.ClientTypes

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Servant.Client           (Scheme (..))
import qualified Servant.Client           as Client


gitHubScheme    :: Scheme
gitHubScheme    = Https


gitHubOAuthHost :: String
gitHubOAuthHost = "github.com"


gitHubApiHost :: String
gitHubApiHost = "api.github.com"


gitHubPort :: Int
gitHubPort = 443


mediaTypeApplicationJson :: Maybe String
mediaTypeApplicationJson = Just "application/json"


mediaTypeGitHubApi :: Maybe String
mediaTypeGitHubApi = Just "application/vnd.github.v3+json"


userAgent :: Maybe String
userAgent = Just "elm-lang.de"


pathAccessToken :: String
pathAccessToken = "/login/oauth/access_token"


pathUser :: String
pathUser = "/user"


convertCodeToAccessToken ::
  GitHubOAuthRequest
  -> IO (Either Client.ServantError GitHubOAuthResponse)
convertCodeToAccessToken gitHubAuthRequest = do
  manager <- newManager tlsManagerSettings
  let
    url = urlForAuthPath pathAccessToken
    clientEnv = Client.ClientEnv manager url
  response <-
    Client.runClientM (postAccessToken gitHubAuthRequest) clientEnv
  return response


postAccessToken ::
  GitHubOAuthRequest
  -> Client.ClientM GitHubOAuthResponse
postAccessToken gitHubAuthRequest = do
  response <- gitHubAuthClient mediaTypeApplicationJson gitHubAuthRequest
  return response


gitHubAuthClient ::
  Maybe String                          -- accept header
  -> GitHubOAuthRequest                 -- request body
  -> Client.ClientM GitHubOAuthResponse -- the resulting client action
gitHubAuthClient = Client.client ClientAPI.gitHubOAuthClientApi


fetchAuthenticatedUser ::
  String
  -> IO (Either Client.ServantError GitHubUserResponse)
fetchAuthenticatedUser accessToken = do
  manager <- newManager tlsManagerSettings
  let
    url = urlForPath pathUser
    clientEnv = Client.ClientEnv manager url
  response <-
    Client.runClientM (getAuthenticatedUser accessToken) clientEnv
  return response


getAuthenticatedUser :: String -> Client.ClientM GitHubUserResponse
getAuthenticatedUser accessToken = do
  response <- gitHubUserClient  (Just accessToken)
  return response


gitHubUserClient ::
  Maybe String                          -- access token query param
  -> Client.ClientM GitHubUserResponse  -- the resulting client action
gitHubUserClient =
  Client.client ClientAPI.gitHubUserClientApi userAgent mediaTypeGitHubApi


urlForAuthPath :: String -> Client.BaseUrl
urlForAuthPath path =
  Client.BaseUrl
  { Client.baseUrlScheme = gitHubScheme
  , Client.baseUrlHost   = gitHubOAuthHost
  , Client.baseUrlPort   = gitHubPort
  , Client.baseUrlPath   = path
  }


urlForPath :: String -> Client.BaseUrl
urlForPath path =
  Client.BaseUrl
  { Client.baseUrlScheme = gitHubScheme
  , Client.baseUrlHost   = gitHubApiHost
  , Client.baseUrlPort   = gitHubPort
  , Client.baseUrlPath   = path
  }


