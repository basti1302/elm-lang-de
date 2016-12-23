{-# LANGUAGE DeriveGeneric #-}

module AppBootstrap.Response where

import           Data.Aeson
import           GHC.Generics


data AppBootstrapResponse = AppBootstrapResponse
  { gitHubClientId :: Maybe String
  } deriving (Eq, Show, Generic)


instance ToJSON AppBootstrapResponse

