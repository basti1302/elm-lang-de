{-# LANGUAGE DeriveGeneric #-}

module Profile.Response where

import           Data.Aeson
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           Data.UUID       (UUID)
import           Data.UUID.Aeson ()
import           GHC.Generics


data ProfileResponse = ProfileResponse
  { id             :: UUID
  , name           :: Text
  , urlFragment    :: Text
  , job            :: Text
  , bio            :: Text
  , available      :: Bool
  , zipCode        :: Text
  , city           :: Text
  , country        :: Text
  , email          :: Text
  , homepage       :: Text
  , githubUsername :: Text
  , twitterHandle  :: Text
  , createdAt      :: UTCTime
  , messages       :: [Text]
  } deriving (Eq, Show, Generic)


instance ToJSON ProfileResponse

