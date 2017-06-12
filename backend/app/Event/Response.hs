{-# LANGUAGE DeriveGeneric #-}

module Event.Response where


import           Profile.Response (ProfileResponse)

import           Data.Aeson
import           Data.Text        (Text)
import           Data.Time        (UTCTime)
import           Data.UUID        (UUID)
import           Data.UUID.Aeson  ()
import           GHC.Generics


data EventResponse = EventResponse
  { id          :: UUID
  , title       :: Text
  , url         :: Text
  , description :: Text
  , createdBy   :: Maybe ProfileResponse
  , createdAt   :: UTCTime
  , messages    :: [Text]
  } deriving (Eq, Show, Generic)


instance ToJSON EventResponse

