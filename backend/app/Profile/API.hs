{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Profile.API (ProfileAPI) where

import           Profile.ProfileHeadResponse (ProfileHeadResponse)
import           Profile.Request             (ProfileRequest)
import           Profile.Response            (ProfileResponse)

import           Data.Text                   (Text)
import           Servant

type ProfileAPI =
  -- GET /api/profiles
     Get '[JSON] [ProfileHeadResponse]
  -- GET /api/profiles/{profile-url-fragment}
  :<|> Capture "profileUrlFragment" Text
      :> Get '[JSON] ProfileResponse
  -- PUT /api/profiles/{profile-url-fragment}
  :<|> Capture "profileUrlFragment" Text
      :> ReqBody '[JSON] ProfileRequest
      :> Put '[JSON] ProfileResponse
  -- DELETE /api/profile/{profile-url-fragment}
  :<|> Capture "profileUrlFragment" Text
       :> Delete '[JSON] NoContent

