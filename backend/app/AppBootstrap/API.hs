{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module AppBootstrap.API (AppBootstrapAPI) where

import           AppBootstrap.Response as AppBootstrapResponse

import           Data.Text             (Text)
import           Servant


type AppBootstrapAPI =
  -- GET /api/app-bootstrap
     Header "Cookie" Text
  :> Get '[JSON] AppBootstrapResponse


