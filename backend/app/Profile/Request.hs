{-# LANGUAGE DeriveGeneric #-}

module Profile.Request where


import qualified Util.Validation as Validation

import           Data.Aeson
import           Data.Text       (Text)
import           GHC.Generics


data ProfileRequest = ProfileRequest
  { name            :: Maybe Text
  , urlFragment     :: Maybe Text
  , job             :: Maybe Text
  , bio             :: Maybe Text
  , available       :: Maybe Bool
  , zipCode         :: Maybe Text
  , city            :: Maybe Text
  , country         :: Maybe Text
  , email           :: Maybe Text
  , homepage        :: Maybe Text
  , gitHubUsername  :: Maybe Text
  , gitHubAvatarUrl :: Maybe Text
  , gravatarId      :: Maybe Text
  , twitterHandle   :: Maybe Text
  } deriving (Eq, Show, Generic)


instance FromJSON ProfileRequest


instance Validation.HasOptionalEMail ProfileRequest where
  getEMail = email
  setEMail mail request = request { email = mail }


instance Validation.HasOptionalURL ProfileRequest where
  getURL = homepage
  setURL url request = request { homepage = url }

