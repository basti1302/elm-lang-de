{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Profile.API (ProfileAPI) where

import           Profile.ProfileHeadResponse (ProfileHeadResponse)
import           Profile.Request             (ProfileRequest)
import           Profile.Response            (ProfileResponse)

import           Data.Text                   (Text)
import           Data.UUID                   (UUID)
import           Servant

type ProfileAPI =
  -- GET /api/profiles
     Get '[JSON] [ProfileHeadResponse]
  -- GET /api/profiles/{profile-url-fragment}
  :<|> Capture "profileUrlFragment" Text
      :> Get '[JSON] ProfileResponse
  -- PUT /api/profiles/{profile-id}
  :<|> Capture "profileId" UUID
      :> ReqBody '[JSON] ProfileRequest
      :> Put '[JSON] ProfileResponse
  -- DELETE /api/profile/{profile-id}
  :<|> Capture "profileId" UUID
       :> Delete '[JSON] NoContent

