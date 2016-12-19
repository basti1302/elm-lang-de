{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module StaticPage.Data where

import           GHC.Generics
import           Servant.EDE  (ToObject)


data StaticPageData = StaticPageData
  { cacheBuster   :: String
  } deriving (Eq, Show, Generic)


instance ToObject StaticPageData

