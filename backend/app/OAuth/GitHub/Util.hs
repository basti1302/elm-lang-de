{-# LANGUAGE OverloadedStrings #-}

module OAuth.GitHub.Util
  ( getOrCreateProfileWithAccessToken
  , getOrCreateProfileWithGitHubOAuthResponse
  )
  where

import           Database.StatementMap
import           ElmLangDe.Util               (createUuid)
import qualified OAuth.GitHub.ClientConnector as GitHubClientConnector
import           OAuth.GitHub.ClientTypes     (GitHubOAuthResponse,
                                               GitHubUserResponse)
import qualified OAuth.GitHub.ClientTypes     as GitHub
import           Profile.Model                (Profile (Profile))
import qualified Profile.Model                as Profile
import qualified Profile.Server               (signUp)
import qualified Profile.SQL

import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time.Clock              (UTCTime)
import qualified Data.Time.Clock              as Clock
import           Data.UUID                    (UUID)
import           Database.HDBC                (IConnection)
import           Servant


getOrCreateProfileWithGitHubOAuthResponse ::
  IConnection connection =>
  DbConnection connection
  -> GitHubOAuthResponse
  -> IO (Either ServantErr Profile)
getOrCreateProfileWithGitHubOAuthResponse dbConnection authResponse =
  let
    accessToken = GitHub.access_token authResponse
  in
    getOrCreateProfileWithAccessToken dbConnection accessToken


getOrCreateProfileWithAccessToken ::
  IConnection connection =>
  DbConnection connection
  -> String
  -> IO (Either ServantErr Profile)
getOrCreateProfileWithAccessToken dbConnection accessToken = do
  gitHubUserResponse <-
    GitHubClientConnector.fetchAuthenticatedUser accessToken
  case gitHubUserResponse of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return $ Left err500
    Right userResponse -> do
      loadOrCreateProfile dbConnection userResponse


loadOrCreateProfile ::
  IConnection connection =>
  DbConnection connection
  -> GitHubUserResponse
  -> IO (Either ServantErr Profile)
loadOrCreateProfile dbConnection userResponse = do
  let
    gitHubLogin = GitHub.login userResponse
  maybeProfile <-
    Profile.SQL.profileByGitHubLogin dbConnection gitHubLogin
  case maybeProfile of
    Just existingProfile -> do
      return $ Right existingProfile
    Nothing -> do
      profileId <- createUuid
      createdAt <- Clock.getCurrentTime
      let
        newProfile = initialProfileFromGitHubUser profileId createdAt userResponse
      Profile.Server.signUp dbConnection newProfile
      return $ Right newProfile


initialProfileFromGitHubUser ::
  UUID
  -> UTCTime
  -> GitHubUserResponse
  -> Profile
initialProfileFromGitHubUser profileId createdAt userResponse =
  let
    login = GitHub.login userResponse
  in
    Profile
    { Profile.id                = profileId
    , Profile.name              = fromMaybe login (GitHub.name userResponse)
    , Profile.urlFragment       = Just login
    , Profile.job               = filterEmpty $ GitHub.company     userResponse
    , Profile.bio               = filterEmpty $ GitHub.bio         userResponse
    , Profile.available         = fromMaybe False $ GitHub.hireable userResponse
    , Profile.zipCode           = Nothing
    , Profile.city              = filterEmpty $ GitHub.location    userResponse
    , Profile.country           = Nothing
    , Profile.email             = filterEmpty $ GitHub.email       userResponse
    , Profile.homepage          = filterEmpty $ GitHub.blog        userResponse
    , Profile.signUpMethod      = "GitHub"
    , Profile.gitHubOAuthLogin  = login
    , Profile.gitHubUsername    = Just $ login
    , Profile.gitHubAvatarUrl   = filterEmpty $ GitHub.avatar_url  userResponse
    , Profile.gravatarId        = filterEmpty $ GitHub.gravatar_id userResponse
    , Profile.twitterHandle     = Nothing
    , Profile.createdAt         = createdAt
    }


filterEmpty :: Maybe Text -> Maybe Text
filterEmpty mt =
  case mt of
    Nothing -> Nothing
    Just t  -> if T.null t then Nothing else Just t

