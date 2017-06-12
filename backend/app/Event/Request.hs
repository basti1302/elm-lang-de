{-# LANGUAGE DeriveGeneric #-}

module Event.Request where


import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics


data EventRequest = EventRequest
  { title       :: Text
  , url         :: Text
  , description :: Text
  } deriving (Eq, Show, Generic)


instance FromJSON EventRequest

