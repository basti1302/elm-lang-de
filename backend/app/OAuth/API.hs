{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module OAuth.API (OAuthAPI) where

import           OAuth.GitHub.API (OAuthGitHubAPI)

import           Servant


type OAuthAPI =
       "github"
       :> OAuthGitHubAPI

