{-# LANGUAGE DataKinds #-}

module SignOut.Server (signOutServer) where

import           AccessToken.Util           (deleteAccessTokenCookie)
import           SignOut.API                (SignOutAPI)
import qualified Util.Config                as Config

import           Control.Monad.Trans.Except
import           Servant


signOutServer ::
  Config.WebConfig
  -> Server SignOutAPI
signOutServer =
  postSignOut


postSignOut ::
  Config.WebConfig
  -> ExceptT ServantErr IO
       (Headers '[Header "Set-Cookie" String] NoContent)
postSignOut webConfig = do
  return $  addHeader (deleteAccessTokenCookie webConfig) NoContent

