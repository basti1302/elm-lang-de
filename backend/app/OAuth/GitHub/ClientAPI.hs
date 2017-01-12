{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module OAuth.GitHub.ClientAPI
  ( gitHubOAuthClientApi
  , gitHubUserClientApi
  ) where

import           OAuth.GitHub.ClientTypes

import           Servant


-- | This API describes what we need to send to GH after receiving the OAuth
-- code and what we get from GH in response (the access token).
type GitHubOAuthClientAPI =
     Header "Accept" String
  :> ReqBody '[FormUrlEncoded] GitHubOAuthRequest
  :> Post '[JSON] GitHubOAuthResponse


gitHubOAuthClientApi :: Proxy GitHubOAuthClientAPI
gitHubOAuthClientApi = Proxy


type GitHubUserClientAPI =
     Header "Accept" String
  :> Header "User-Agent" String
  :> QueryParam "access_token" String
  :> Get '[JSON] GitHubUserResponse


gitHubUserClientApi :: Proxy GitHubUserClientAPI
gitHubUserClientApi = Proxy

