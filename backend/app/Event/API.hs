{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Event.API (EventAPI) where

import           Event.EventHeadResponse (EventHeadResponse)
import           Event.Request           (EventRequest)
import           Event.Response          (EventResponse)

import           Data.UUID               (UUID)
import           Servant

type EventAPI =
  -- GET /api/events
     Get '[JSON] [EventHeadResponse]
  -- GET /api/events/{event-id}
  :<|> Capture "eventId" UUID
      :> Get '[JSON] EventResponse
  -- POST /api/events
  :<|> ReqBody '[JSON] EventRequest
      :> Post '[JSON] EventResponse
  -- PUT /api/events/{event-id}
  :<|> Capture "eventId" UUID
      :> ReqBody '[JSON] EventRequest
      :> Put '[JSON] EventResponse
  -- DELETE /api/event/{event-id}
  :<|> Capture "eventId" UUID
       :> Delete '[JSON] NoContent

