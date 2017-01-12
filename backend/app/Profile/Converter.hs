{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Profile.Converter
  ( modelToHead
  , modelToResponse
  , requestToModel
  , responseForBadRequest
  ) where


import           Profile.Model               (Profile (Profile))
import qualified Profile.Model               as Profile
import           Profile.ProfileHeadResponse (ProfileHeadResponse (ProfileHeadResponse))
import qualified Profile.ProfileHeadResponse as ProfileHeadResponse
import           Profile.Request             (ProfileRequest)
import qualified Profile.Request             as ProfileRequest
import           Profile.Response            (ProfileResponse (ProfileResponse))
import qualified Profile.Response            as ProfileResponse

import           Data.Maybe                  as Maybe
import           Data.Text                   (Text)


requestToModel ::
  Profile
  -> ProfileRequest
  -> Profile
requestToModel existingProfile request =
  Profile
  { Profile.id                = Profile.id                      existingProfile
  , Profile.name              = fm $ ProfileRequest.name        request
  , Profile.urlFragment       = ProfileRequest.urlFragment      request
  , Profile.job               = ProfileRequest.job              request
  , Profile.bio               = ProfileRequest.bio              request
  , Profile.available         =
      Maybe.fromMaybe False $ ProfileRequest.available request
  , Profile.zipCode           = ProfileRequest.zipCode          request
  , Profile.city              = ProfileRequest.city             request
  , Profile.country           = ProfileRequest.country          request
  , Profile.email             = ProfileRequest.email            request
  , Profile.homepage          = ProfileRequest.homepage         request
  , Profile.signUpMethod      = Profile.signUpMethod            existingProfile
  , Profile.gitHubOAuthLogin  = Profile.gitHubOAuthLogin        existingProfile
  , Profile.gitHubUsername    = ProfileRequest.gitHubUsername   request
  , Profile.gitHubAvatarUrl   = ProfileRequest.gitHubAvatarUrl  request
  , Profile.gravatarId        = ProfileRequest.gravatarId       request
  , Profile.twitterHandle     = ProfileRequest.twitterHandle    request
  , Profile.createdAt         = Profile.createdAt               existingProfile
  }


modelToResponse :: Profile -> ProfileResponse
modelToResponse profile =
  ProfileResponse
  { ProfileResponse.id              =      Profile.id              profile
  , ProfileResponse.name            =      Profile.name            profile
  , ProfileResponse.urlFragment     = fm $ Profile.urlFragment     profile
  , ProfileResponse.job             = fm $ Profile.job             profile
  , ProfileResponse.bio             = fm $ Profile.bio             profile
  , ProfileResponse.available       =      Profile.available       profile
  , ProfileResponse.zipCode         = fm $ Profile.zipCode         profile
  , ProfileResponse.city            = fm $ Profile.city            profile
  , ProfileResponse.country         = fm $ Profile.country         profile
  , ProfileResponse.email           = fm $ Profile.email           profile
  , ProfileResponse.homepage        = fm $ Profile.homepage        profile
  , ProfileResponse.gitHubUsername  = fm $ Profile.gitHubUsername  profile
  , ProfileResponse.gitHubAvatarUrl = fm $ Profile.gitHubAvatarUrl profile
  , ProfileResponse.gravatarId      = fm $ Profile.gravatarId      profile
  , ProfileResponse.twitterHandle   = fm $ Profile.twitterHandle   profile
  , ProfileResponse.createdAt       =      Profile.createdAt       profile
  , ProfileResponse.messages        = []
  }


responseForBadRequest ::
  Profile
  -> [Text]
  -> ProfileRequest
  -> ProfileResponse
responseForBadRequest existingProfile messages request =
  ProfileResponse
  { ProfileResponse.id              = Profile.id existingProfile
  , ProfileResponse.name            = fm $ ProfileRequest.name           request
  , ProfileResponse.urlFragment     = fm $ ProfileRequest.urlFragment    request
  , ProfileResponse.job             = fm $ ProfileRequest.job            request
  , ProfileResponse.bio             = fm $ ProfileRequest.bio            request
  , ProfileResponse.available       =
      Maybe.fromMaybe False (ProfileRequest.available request)
  , ProfileResponse.zipCode         = fm $ ProfileRequest.zipCode        request
  , ProfileResponse.city            = fm $ ProfileRequest.city           request
  , ProfileResponse.country         = fm $ ProfileRequest.country        request
  , ProfileResponse.email           = fm $ ProfileRequest.email          request
  , ProfileResponse.homepage        = fm $ ProfileRequest.homepage       request
  , ProfileResponse.gitHubUsername  = fm $ ProfileRequest.gitHubUsername request
  , ProfileResponse.gitHubAvatarUrl =
      fm $ ProfileRequest.gitHubAvatarUrl request
  , ProfileResponse.gravatarId      = fm $ ProfileRequest.gravatarId     request
  , ProfileResponse.twitterHandle   = fm $ ProfileRequest.twitterHandle  request
  , ProfileResponse.createdAt       = Profile.createdAt existingProfile
  , ProfileResponse.messages        = messages
  }


modelToHead :: Profile -> ProfileHeadResponse
modelToHead profile =
  ProfileHeadResponse
  { ProfileHeadResponse.id             = Profile.id               profile
  , ProfileHeadResponse.name           = Profile.name             profile
  , ProfileHeadResponse.urlFragment    = fm $ Profile.urlFragment profile
  , ProfileHeadResponse.createdAt      = Profile.createdAt        profile
  }


fm :: Maybe Text -> Text
fm = Maybe.fromMaybe ""

