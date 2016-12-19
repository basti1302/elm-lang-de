{-# LANGUAGE OverloadedStrings #-}

module AppPage.Server (appPageServer) where

import           AppPage.API       (AppPageAPI)
import           StaticPage.Server (renderStaticPage)

import           Servant


appPageServer :: Server AppPageAPI
appPageServer = renderStaticPage

