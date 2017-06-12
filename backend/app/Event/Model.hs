{-# LANGUAGE DeriveGeneric #-}

module Event.Model where

import           Database.UUIDConversion ()

import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Data.UUID               (UUID)
import           Database.HDBC           (SqlValue, fromSql)
import           GHC.Generics
import           Prelude                 hiding (id)


data Event = Event
  { -- | the primary key
    id          :: UUID
  , title       :: Maybe Text
    -- | The external url for the event (Meetup url or similar)
  , url         :: Maybe Text
    -- a longer description for the event
  , description :: Maybe Text
    -- | id of the user who created the event
  , createdBy   :: Maybe UUID
    -- | timestamp when the event has been created on elm-lang.de
  , createdAt   :: UTCTime
  } deriving (Eq, Show, Generic)


fromRow :: Maybe [SqlValue] -> Maybe Event
fromRow values =
  case values of
    Just [ _id
         , _title
         , _url
         , _description
         , _created_by
         , _created_at
         ] ->
            Just Event
             { id               = fromSql _id
             , title            = fromSql _title
             , url              = fromSql _url
             , description      = fromSql _description
             , createdBy        = fromSql _created_by
             , createdAt        = fromSql _created_at
             }
    _ -> Nothing

