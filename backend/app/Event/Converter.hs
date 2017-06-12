{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Event.Converter
  ( modelToHead
  , modelToResponse
  , requestToModel
  , requestToNewModel
  , responseForBadRequest
  ) where


import           Event.EventHeadResponse (EventHeadResponse (EventHeadResponse))
import qualified Event.EventHeadResponse as EventHeadResponse
import           Event.Model             (Event (Event))
import qualified Event.Model             as Event
import           Event.Request           (EventRequest)
import qualified Event.Request           as EventRequest
import           Event.Response          (EventResponse (EventResponse))
import qualified Event.Response          as EventResponse
import qualified Profile.Converter
import           Profile.Model           (Profile)

import           Data.Maybe              as Maybe
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Data.UUID               (UUID)


requestToNewModel ::
  UUID
  -> Maybe UUID
  -> UTCTime
  -> EventRequest
  -> Event
requestToNewModel eventId createdById createdAt request =
  Event
  { Event.id          = eventId
  , Event.title       = Just $ EventRequest.title       request
  , Event.url         = Just $ EventRequest.url         request
  , Event.description = Just $ EventRequest.description request
  , Event.createdBy   = createdById
  , Event.createdAt   = createdAt
  }


requestToModel ::
  Event
  -> EventRequest
  -> Event
requestToModel existingEvent request =
  Event
  { Event.id          = Event.id                        existingEvent
  , Event.title       = Just $ EventRequest.title       request
  , Event.url         = Just $ EventRequest.url         request
  , Event.description = Just $ EventRequest.description request
  , Event.createdBy   = Event.createdBy                 existingEvent
  , Event.createdAt   = Event.createdAt                 existingEvent
  }


modelToResponse :: Event -> Maybe Profile -> EventResponse
modelToResponse event createdBy =
  let
    createdByProfileResponse =
      case createdBy of
        Just profile -> Just $ Profile.Converter.modelToResponse profile
        Nothing      -> Nothing
  in
    EventResponse
    { EventResponse.id          = Event.id               event
    , EventResponse.title       = fm $ Event.title       event
    , EventResponse.url         = fm $ Event.url         event
    , EventResponse.description = fm $ Event.description event
    , EventResponse.createdAt   = Event.createdAt event
    , EventResponse.createdBy   = createdByProfileResponse
    , EventResponse.messages    = []
    }


responseForBadRequest ::
  Event
  -> [Text]
  -> EventRequest
  -> EventResponse
responseForBadRequest existingEvent messages request =
  EventResponse
  { EventResponse.id          = Event.id                 existingEvent
  , EventResponse.title       = EventRequest.title       request
  , EventResponse.url         = EventRequest.url         request
  , EventResponse.description = EventRequest.description request
  , EventResponse.createdAt   = Event.createdAt          existingEvent
  , EventResponse.createdBy   = Nothing
  , EventResponse.messages    = messages
  }


modelToHead :: Event -> EventHeadResponse
modelToHead event =
  EventHeadResponse
  { EventHeadResponse.id          = Event.id          event
  , EventHeadResponse.title       = fm $ Event.title  event
  , EventHeadResponse.url         = fm $ Event.url    event
  }


fm :: Maybe Text -> Text
fm = Maybe.fromMaybe ""

