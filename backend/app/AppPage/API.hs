{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module AppPage.API (AppPageAPI) where

import           StaticPage.Data

import           Servant
import           Servant.EDE     (HTML)

type AppPageAPI = Get '[HTML "index.tpl"] StaticPageData

