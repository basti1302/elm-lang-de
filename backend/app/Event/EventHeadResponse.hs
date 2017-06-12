{-# LANGUAGE DeriveGeneric #-}

module Event.EventHeadResponse where

import           Data.Aeson
import           Data.Text       (Text)
import           Data.UUID       (UUID)
import           Data.UUID.Aeson ()
import           GHC.Generics


data EventHeadResponse = EventHeadResponse
  { id    :: UUID
  , title :: Text
  , url   :: Text
  } deriving (Eq, Show, Generic)


instance ToJSON EventHeadResponse

