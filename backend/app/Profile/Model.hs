{-# LANGUAGE DeriveGeneric #-}

module Profile.Model where

import           Database.UUIDConversion ()

import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Data.UUID               (UUID)
import           Database.HDBC           (SqlValue, fromSql)
import           GHC.Generics
import           Prelude                 hiding (id)


data Profile = Profile
  { -- | the primary key
    id             :: UUID
    -- | the full name of the developer
  , name           :: Text
    -- | The last url fragment of the profile's URL on elm-lang.de.
    --   The profile will be available at
    --   https://elm-lang.de/entwickler#{url-fragment}
  , urlFragment    :: Maybe Text
    -- | a free form field for either the employee or the the occupation of the
    --   developer (examples: "Front End Dev at Company XYZ", "Freelancer", ...)
  , job            :: Maybe Text
    -- | whatever the developer wants to tell the world about xyrself
  , bio            :: Maybe Text
    -- | available for Elm projects
  , available      :: Bool
  , zipCode        :: Maybe Text
  , city           :: Maybe Text
  , country        :: Maybe Text
  , email          :: Maybe Text
  , homepage       :: Maybe Text
  , githubUsername :: Maybe Text
  , twitterHandle  :: Maybe Text
    -- | timestamp when the profile has been created on elm-lang.de
  , createdAt      :: UTCTime
  } deriving (Eq, Show, Generic)


fromRow :: Maybe [SqlValue] -> Maybe Profile
fromRow values =
  case values of
    Just [ _id
         , _name
         , _url_fragment
         , _job
         , _bio
         , _available
         , _zip_code
         , _city
         , _country
         , _email
         , _homepage
         , _github_username
         , _twitter_handle
         , _created_at
         ] ->
            Just Profile
             { id             = fromSql _id
             , name           = fromSql _name
             , urlFragment    = fromSql _url_fragment
             , job            = fromSql _job
             , bio            = fromSql _bio
             , available      = fromSql _available
             , zipCode        = fromSql _zip_code
             , city           = fromSql _city
             , country        = fromSql _country
             , email          = fromSql _email
             , homepage       = fromSql _homepage
             , githubUsername = fromSql _github_username
             , twitterHandle  = fromSql _twitter_handle
             , createdAt      = fromSql _created_at
             }
    _ -> Nothing

