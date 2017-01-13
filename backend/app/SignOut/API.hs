{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module SignOut.API (SignOutAPI) where

import           Servant


type SignOutAPI =
     -- POST /api/sign-out
     Post '[JSON] (Headers '[Header "Set-Cookie" String] NoContent)

