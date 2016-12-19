{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Profile.API (ProfileAPI) where

import           Profile.ProfileHeadResponse (ProfileHeadResponse)
import           Profile.Request             (ProfileRequest)
import           Profile.Response            (ProfileResponse)

import           Data.UUID
import           Servant

type ProfileAPI =
  -- GET /api/profiles
     Get '[JSON] [ProfileHeadResponse]
  -- GET /api/profiles/{profile-id}
  :<|> Capture "profileId" UUID
      :> Get '[JSON] ProfileResponse
  -- POST /api/profiles
  :<|> ReqBody '[JSON] ProfileRequest
      :> Post '[JSON] ProfileResponse
  -- PUT /api/profiles/{profile-id}
  :<|> Capture "profileId" UUID
      :> ReqBody '[JSON] ProfileRequest
      :> Put '[JSON] ProfileResponse
  -- DELETE /api/profile/{profile-id}
  :<|> Capture "profileId" UUID
       :> Delete '[JSON] NoContent

