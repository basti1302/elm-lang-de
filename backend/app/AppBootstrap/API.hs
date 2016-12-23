{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module AppBootstrap.API (AppBootstrapAPI) where

import           AppBootstrap.Response as AppBootstrapResponse
import           Servant


type AppBootstrapAPI =
  -- GET /api/app-bootstrap
  Get '[JSON] AppBootstrapResponse


