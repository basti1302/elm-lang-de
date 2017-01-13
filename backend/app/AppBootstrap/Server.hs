{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module AppBootstrap.Server (appBootstrapServer) where

import qualified AccessToken.Util
import           AppBootstrap.API           (AppBootstrapAPI)
import           AppBootstrap.Response      (AppBootstrapResponse (NotSignedIn, SignedIn))
import qualified AppBootstrap.Response      as AppBootstrapResponse
import           Database.StatementMap
import qualified OAuth.GitHub.Util
import qualified Profile.Converter
import           Profile.Model              (Profile)
import qualified Util.Config                as Config

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Data.Text                  (Text)
import           Database.HDBC              (IConnection)
import           Servant


appBootstrapServer ::
  IConnection connection =>
  Config.WebConfig
  -> DbConnection connection
  -> Server AppBootstrapAPI
appBootstrapServer =
  get


get ::
  IConnection connection =>
  Config.WebConfig
  -> DbConnection connection
  -> Maybe Text
  -> ExceptT ServantErr IO AppBootstrapResponse
get webConfig dbConnection cookieHeader = do
  let
    accessToken = AccessToken.Util.readAccessTokenFromCookieHeader cookieHeader
  case accessToken of
    Nothing ->
      return $ notSignedIn webConfig
    Just token -> do
      eitherProfileOrError <-
        liftIO $
          OAuth.GitHub.Util.getOrCreateProfileWithAccessToken dbConnection token
      case eitherProfileOrError of
        Left err -> do
          liftIO $
            putStrLn $
            "Error during GitHub OAuth sign in: " ++ (show err)
          return $ notSignedIn webConfig
        Right profile ->
          return $ signedIn webConfig profile


notSignedIn :: Config.WebConfig -> AppBootstrapResponse
notSignedIn webConfig =
  NotSignedIn
  { AppBootstrapResponse.signedIn = False
  , AppBootstrapResponse.gitHubClientId = Config.gitHubClientId webConfig
  , AppBootstrapResponse.gitHubOAuthRedirectUrl =
      Config.gitHubOAuthRedirectUrl webConfig
  }


signedIn :: Config.WebConfig -> Profile -> AppBootstrapResponse
signedIn webConfig profile =
  let
    profileResponse = Profile.Converter.modelToResponse profile
  in
    SignedIn
    { AppBootstrapResponse.signedIn = True
    , AppBootstrapResponse.profile = profileResponse
    , AppBootstrapResponse.gitHubClientId = Config.gitHubClientId webConfig
    , AppBootstrapResponse.gitHubOAuthRedirectUrl =
        Config.gitHubOAuthRedirectUrl webConfig
    }

