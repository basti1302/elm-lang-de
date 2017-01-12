{-# LANGUAGE DeriveGeneric #-}

module OAuth.GitHub.ClientTypes where

import           Data.Aeson
import           Data.Text          (Text)
import           GHC.Generics       hiding (from, to)
import           Web.FormUrlEncoded (ToForm)


data GitHubOAuthRequest = GitHubOAuthRequest
  { client_id     :: Text
  , client_secret :: Text
  , code          :: Text
  } deriving (Eq, Show, Generic)


instance ToForm GitHubOAuthRequest


data GitHubOAuthResponse = GitHubOAuthResponse
  { access_token :: String
  , scope        :: String
  } deriving (Show, Generic)


instance FromJSON GitHubOAuthResponse


-- see https://developer.github.com/v3/users/#get-the-authenticated-user
-- for all fields that GH provides
data GitHubUserResponse = GitHubUserResponse
  { login       :: Text
  , id          :: Integer
  , name        :: Maybe Text
  , email       :: Maybe Text
  , avatar_url  :: Maybe Text
  , gravatar_id :: Maybe Text
  , company     :: Maybe Text
  , blog        :: Maybe Text
  , location    :: Maybe Text
  , hireable    :: Maybe Bool
  , bio         :: Maybe Text
  } deriving (Show, Generic)


instance FromJSON GitHubUserResponse

