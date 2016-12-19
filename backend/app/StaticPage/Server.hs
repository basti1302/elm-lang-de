{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}

module StaticPage.Server (renderStaticPage) where

import qualified ElmLangDe.Util             as Util
import           StaticPage.Data            (StaticPageData (StaticPageData))

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import           Servant


renderStaticPage :: ExceptT ServantErr IO StaticPageData
renderStaticPage = do
  cacheBuster <- liftIO Util.generateCacheBuster
  return $ StaticPageData cacheBuster

