{-# LANGUAGE OverloadedStrings #-}

module OAuth.GitHub.Server (oAuthGitHubServer) where

import           AccessToken.Util             (accessTokenToCookie)
import           Database.StatementMap
import           OAuth.GitHub.API             (OAuthGitHubAPI)
import qualified OAuth.GitHub.ClientConnector as GitHubClientConnector
import           OAuth.GitHub.ClientTypes     (GitHubOAuthRequest (GitHubOAuthRequest))
import qualified OAuth.GitHub.ClientTypes     as GitHub
import           OAuth.GitHub.Util            (getOrCreateProfileWithGitHubOAuthResponse)
import qualified Util.Config                  as Config

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8        as BSC8
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Database.HDBC                (IConnection)
import           Servant


oAuthGitHubServer ::
  IConnection connection =>
  Config.WebConfig
  -> DbConnection connection
  -> Server OAuthGitHubAPI
oAuthGitHubServer webConfig dbConnection =
  get webConfig dbConnection


get ::
  IConnection connection =>
  Config.WebConfig
  -> DbConnection connection
  -> Maybe Text
  -> ExceptT ServantErr IO NoContent
get webConfig dbConnection maybeGithubAuthCode = do
  result <- liftIO $ oAuthHandshake webConfig dbConnection maybeGithubAuthCode
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
  IConnection connection =>
  Config.WebConfig
  -> DbConnection connection
  -> Maybe Text
  -> IO (Either ServantErr String)
oAuthHandshake webConfig dbConnection maybeGithubAuthCode = do
  if not (Config.hasGitHubOAuthConfig webConfig)
    then do
      putStr "GitHub OAuth attempt but GitHub OAuth is not configured (missing \
      \client ID and/or client secret)."
      return $ Left err500
    else do
      case maybeGithubAuthCode of
        Just githubAuthCode -> do
          convertCodeToAccessToken webConfig dbConnection githubAuthCode
        Nothing ->
          return $ Left err500


convertCodeToAccessToken ::
  IConnection connection =>
  Config.WebConfig
  -> DbConnection connection
  -> Text
  -> IO (Either ServantErr String)
convertCodeToAccessToken webConfig dbConnection githubAuthCode = do
  let
    Just clientId     = Config.gitHubClientId     webConfig
    Just clientSecret = Config.gitHubClientSecret webConfig
    gitHubAuthRequest =
      GitHubOAuthRequest
      { GitHub.client_id     = T.pack clientId
      , GitHub.client_secret = T.pack clientSecret
      , GitHub.code          = githubAuthCode
      }
  gitHubOAuthResponse <-
    GitHubClientConnector.convertCodeToAccessToken gitHubAuthRequest
  case gitHubOAuthResponse of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return $ Left err500
    Right authResponse -> do
      _ <- getOrCreateProfileWithGitHubOAuthResponse dbConnection authResponse
      return $ Right $ GitHub.access_token authResponse

