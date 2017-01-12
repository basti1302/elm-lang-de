{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Auth
  ( accessTokenAuthContext
  , lookupUserByAccessToken
  ) where

import           AccessToken.Util                 (readAccessTokenFromRequest)
import           Control.Monad.IO.Class           (liftIO)
import           Database.StatementMap
import           Profile.Model                    (Profile)
import qualified Profile.SQL

import           Network.Wai
import           Servant                          (ServantErr, throwError)
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Server
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)


-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
accessTokenAuthContext ::
  DbConnection connection
  -> Context (AuthHandler Request Profile ': '[])
accessTokenAuthContext dbConnection =
  (accessTokenAuthHandler dbConnection) :. EmptyContext


-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = Profile


-- | The auth handler wraps a function from Request -> Handler Profile
-- we look for the cookie use the value to look up the profile.
accessTokenAuthHandler ::
  DbConnection connection
  -> AuthHandler Request Profile
accessTokenAuthHandler dbConnection =
  let
    handler req =
      case readAccessTokenFromRequest req of
        Nothing ->
          throwError (err401 { errBody = "Missing access token" })
        Just token ->
          lookupUserByAccessToken dbConnection token
  in mkAuthHandler handler


-- | A method that, when given an access token, will return a Profile.
lookupUserByAccessToken ::
  DbConnection connection
  -> String
  -> Handler Profile
lookupUserByAccessToken dbConnection token = do
  result <- liftIO $ lookupIOUserByAccessToken dbConnection token
  case result of
    Right profile -> return profile
    Left err      -> throwError err


lookupIOUserByAccessToken ::
  DbConnection connection
  -> String
  -> IO (Either ServantErr Profile)
lookupIOUserByAccessToken dbConnection token = do
  -- TODO
  -- 1) Use token to fetch email/github id/some unique thing from github
  -- 2) lookup the profile with this unique thing
  maybeProfile <- Profile.SQL.profileByGitHubLogin dbConnection token
  case maybeProfile of
    Just profile -> return $ Right $ profile
    Nothing      -> return $ Left $ err403 { errBody = "Invalid access token" }

