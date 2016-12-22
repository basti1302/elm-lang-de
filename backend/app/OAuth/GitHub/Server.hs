{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module OAuth.GitHub.Server (oAuthGitHubServer) where

import           OAuth.GitHub.API           (OAuthGitHubAPI)
import qualified Util.Config                as Config

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.Binary.Builder
import qualified Data.ByteString.Char8      as BSC8
import qualified Data.ByteString.Lazy
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (DiffTime, NominalDiffTime,
                                             UTCTime (UTCTime))
import qualified Data.Time                  as Time
import           GHC.Generics               hiding (from, to)
import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Servant
import           Servant.Client             (Scheme (..))
import qualified Servant.Client             as Client
import qualified Web.Cookie                 as Cookie
import           Web.FormUrlEncoded         (ToForm)


oAuthGitHubServer ::
  Config.WebConfig
  -> Server OAuthGitHubAPI
oAuthGitHubServer webConfig =
  get webConfig


get ::
  Config.WebConfig
  -> Maybe Text
  -> ExceptT ServantErr IO NoContent
get webConfig maybeGithubAuthCode = do
  result <- liftIO $ oAuthHandshake webConfig maybeGithubAuthCode
  case result of
    Left err ->
      throwE err { errBody = "Das hat leider nicht geklappt :-(" }
    Right accessToken -> do
      -- redirect to Elm app, set access token session cookie
      cookieHeader <- liftIO $ accessTokenToCookie webConfig accessToken
      let
        redirect =
          err303 {
            errHeaders =
              [ ("Location", "/")
              , ("Set-Cookie", BSC8.pack cookieHeader)
              ]
          }
      throwE redirect


oAuthHandshake ::
  Config.WebConfig
  -> Maybe Text
  -> IO (Either ServantErr String)
oAuthHandshake webConfig maybeGithubAuthCode = do
  if not (Config.hasGitHubOAuthConfig webConfig)
    then do
      putStr "GitHub OAuth attempt but GitHub OAuth is not configured (missing \
      \client ID and/or client secret)."
      return $ Left err500
    else do
      case maybeGithubAuthCode of
        Just githubAuthCode -> do
          let
            Just clientId     = Config.gitHubClientId     webConfig
            Just clientSecret = Config.gitHubClientSecret webConfig
            gitHubAuthRequest =
              GitHubOAuthRequest
              { client_id     = T.pack clientId
              , client_secret = T.pack clientSecret
              , code          = githubAuthCode
              }
          gitHubAuthResponse <- convertCodeToAccessToken gitHubAuthRequest
          case gitHubAuthResponse of
            Left err -> do
              putStrLn $ "Error: " ++ show err
              return $ Left err500
            Right response -> do
              let
                accessToken = access_token response
              putStrLn $ "GH token: " ++ (show accessToken)
              return $ Right accessToken
        Nothing ->
          return $ Left err500


convertCodeToAccessToken ::
  GitHubOAuthRequest
  -> IO (Either Client.ServantError GitHubOAuthResponse)
convertCodeToAccessToken gitHubAuthRequest = do
  let baseUrl =
        Client.BaseUrl
        { Client.baseUrlScheme = Https
        , Client.baseUrlHost   = "github.com"
        , Client.baseUrlPort   = 443
        , Client.baseUrlPath   = "/login/oauth/access_token"
        }
  manager <- newManager tlsManagerSettings
  let
    clientEnv = (Client.ClientEnv manager baseUrl)
  response <-
    Client.runClientM (postCodeToGetAccessToken gitHubAuthRequest) clientEnv
  return response


data GitHubOAuthRequest = GitHubOAuthRequest
  { client_id     :: Text
  , client_secret :: Text
  , code          :: Text
  } deriving (Eq, Show, Generic)


instance ToForm GitHubOAuthRequest


data GitHubOAuthResponse = GitHubOAuthResponse
  { access_token :: String
  } deriving (Show, Generic)


instance FromJSON GitHubOAuthResponse


-- | This API describes what we need to send to GH after receiving the OAuth
-- code and what we get from GH in response (the access token).
type GitHubClientAPI =
     Header "Accept" String
  :> ReqBody '[FormUrlEncoded] GitHubOAuthRequest
  :> Post '[JSON] GitHubOAuthResponse


gitHubClientApi :: Proxy GitHubClientAPI
gitHubClientApi = Proxy


gitHubAuthClient ::
  Maybe String                          -- accept header
  -> GitHubOAuthRequest                 -- request body
  -> Client.ClientM GitHubOAuthResponse -- the resulting client action
gitHubAuthClient = Client.client gitHubClientApi


postCodeToGetAccessToken ::
  GitHubOAuthRequest
  -> Client.ClientM GitHubOAuthResponse
postCodeToGetAccessToken gitHubAuthRequest = do
  response <- gitHubAuthClient (Just "application/json") gitHubAuthRequest
  return response


accessTokenCookieName :: String
accessTokenCookieName = "github-access-token"


accessTokenToCookie ::
  Config.WebConfig
  -> String
  -> IO String
accessTokenToCookie webConfig accessToken = do
  now <- Time.getCurrentTime
  let
    validUntil = Time.addUTCTime accessTokenValidDuration now
    cookie = Cookie.def
             { Cookie.setCookieName     = BSC8.pack accessTokenCookieName
             , Cookie.setCookieValue    = BSC8.pack accessToken
             , Cookie.setCookiePath     = Just "/"
             , Cookie.setCookieSecure   =
                 not $ Config.secureCookiesDisabled webConfig
             , Cookie.setCookieSameSite = Just Cookie.sameSiteStrict
             , Cookie.setCookieHttpOnly = True
             , Cookie.setCookieExpires  = Just validUntil
             , Cookie.setCookieMaxAge   = Just $ diffUTCTime validUntil now
             }
    cookieHeaderLazy = (Data.Binary.Builder.toLazyByteString .
                    Cookie.renderSetCookie)
                    cookie
  return $ BSC8.unpack $ Data.ByteString.Lazy.toStrict cookieHeaderLazy


diffUTCTime :: UTCTime -> UTCTime -> DiffTime
diffUTCTime (UTCTime daysA secondsA) (UTCTime daysB secondsB) =
   let
     differenceInDays :: Integer
     differenceInDays = (Time.diffDays daysA daysB)
     differenceInDaysConvertedToSeconds :: Integer
     differenceInDaysConvertedToSeconds = differenceInDays * 86400
     differenceInDaysConvertedToSecondsAsDiffTime :: DiffTime
     differenceInDaysConvertedToSecondsAsDiffTime =
       Time.secondsToDiffTime differenceInDaysConvertedToSeconds
     differenceInTimeOfDay :: DiffTime
     differenceInTimeOfDay = secondsB - secondsA
   in
     differenceInDaysConvertedToSecondsAsDiffTime + differenceInTimeOfDay


accessTokenValidDuration :: NominalDiffTime
accessTokenValidDuration =
  -- let the access token be valid for 365 days
  365 {- days    -} *
   24 {- hours   -} *
   60 {- minutes -} *
   60 {- seconds -}

