module Html.Server (htmlServer) where

import           AppPage.API    (AppPageAPI)
import           AppPage.Server (appPageServer)
import           Html.API       (HtmlAPI)

import           Servant


htmlServer ::
  Server HtmlAPI
htmlServer =
  let
    appPageAPIHandler :: Server AppPageAPI
    appPageAPIHandler =
      appPageServer
  in
    appPageAPIHandler

