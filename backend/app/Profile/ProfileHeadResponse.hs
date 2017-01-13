{-# LANGUAGE DeriveGeneric #-}

module Profile.ProfileHeadResponse where

import           Data.Aeson
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           Data.UUID       (UUID)
import           Data.UUID.Aeson ()
import           GHC.Generics


data ProfileHeadResponse = ProfileHeadResponse
  { id              :: UUID
  , name            :: Text
  , urlFragment     :: Text
  , job             :: Text
  , city            :: Text
  , gitHubAvatarUrl :: Text
  , gravatarId      :: Text
  , createdAt       :: UTCTime
  } deriving (Eq, Show, Generic)


instance ToJSON ProfileHeadResponse

