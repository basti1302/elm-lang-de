{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module OAuth.GitHub.API (OAuthGitHubAPI) where

import           Data.Text (Text)
import           Servant


type OAuthGitHubAPI =
   -- GET /oauth/github
     QueryParam "code" Text
  :> Get '[PlainText] NoContent

