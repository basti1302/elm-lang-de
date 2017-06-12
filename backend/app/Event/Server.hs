{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Event.Server
  ( eventServer
  ) where

import           Database.StatementMap
import qualified ElmLangDe.Util             as Util
import           Event.API                  (EventAPI)
import qualified Event.Converter            as EventConverter
import           Event.EventHeadResponse    (EventHeadResponse)
import           Event.Model                (Event)
import qualified Event.Model                as Event
import           Event.Request              (EventRequest)
import           Event.Response             (EventResponse)
import qualified Event.SQL                  as SQL
import           Profile.Model              (Profile)
import qualified Profile.SQL
import qualified Util.Validation            as Validation

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as JSON (encode)
import           Data.Text                  (Text)
import qualified Data.Time                  as Time
import           Data.UUID                  (UUID)
import           Database.HDBC              (IConnection)
import           Servant


eventServer ::
  IConnection connection =>
  DbConnection connection
  -> Server EventAPI
eventServer dbConnection =
       getEvents dbConnection
  :<|> getEvent dbConnection
  :<|> postEvent dbConnection
  :<|> putEvent dbConnection
  :<|> deleteEvent dbConnection


getEvents ::
  IConnection connection =>
  DbConnection connection
  -> ExceptT ServantErr IO [EventHeadResponse]
getEvents dbConnection =
  liftIO $
  withTransaction dbConnection $ \transactedConnection ->
    fetchAllEvents transactedConnection


fetchAllEvents ::
  DbConnection connection
  -> IO [EventHeadResponse]
fetchAllEvents dbConnection = do
  events <- SQL.allEvents dbConnection
  return $ map EventConverter.modelToHead events


getEvent ::
  IConnection connection =>
  DbConnection connection
  -> UUID
  -> ExceptT ServantErr IO EventResponse
getEvent dbConnection eventId = do
  result <- liftIO $
            withTransaction dbConnection $ \transactedConnection ->
              fetchOneEvent transactedConnection eventId
  case result of
    Right event -> return event
    Left err    -> throwE err


fetchOneEvent ::
  DbConnection connection
  -> UUID
  -> IO (Either ServantErr EventResponse)
fetchOneEvent dbConnection eventId = do
  maybeEvent <- SQL.eventById dbConnection eventId
  case maybeEvent of
    Just event -> do
      createdBy <- fetchProfileForEvent dbConnection event
      return $
        Right $ EventConverter.modelToResponse event createdBy
    Nothing -> return $ Left err404


postEvent ::
  IConnection connection =>
  DbConnection connection
  -> EventRequest
  -> ExceptT ServantErr IO EventResponse
postEvent dbConnection eventRequest = do
  result <- liftIO $ withTransaction dbConnection $ \transactedConnection ->
    createEvent
      transactedConnection
      eventRequest
  case result of
    Left  err      -> throwE err
    Right response -> return response


createEvent ::
  DbConnection connection
  -> EventRequest
  -> IO (Either ServantErr EventResponse)
createEvent dbConnection eventRequest = do
  -- TODO figure out by which profile the event has been created
  -- (requires putting this behind auth)
  eventId <- Util.createUuid
  currentTime <- Time.getCurrentTime

  let
    event =
      EventConverter.requestToNewModel
        eventId Nothing currentTime eventRequest
  SQL.newEvent dbConnection event
  -- TODO Handle Nothing
  Just createdEvent <- SQL.eventById dbConnection eventId
  return $
    -- TODO Return correct createdBy
    Right $ EventConverter.modelToResponse createdEvent Nothing


putEvent ::
  IConnection connection =>
  DbConnection connection
  -> UUID
  -> EventRequest
  -> ExceptT ServantErr IO EventResponse
putEvent dbConnection eventId eventRequest = do
  result <- liftIO $ withTransaction dbConnection $ \transactedConnection ->
    updateEvent
      transactedConnection
      eventId
      eventRequest
  case result of
    Left  err      -> throwE err
    Right response -> return response


updateEvent ::
  DbConnection connection
  -> UUID
  -> EventRequest
  -> IO (Either ServantErr EventResponse)
updateEvent dbConnection eventId eventRequest = do
  maybeEvent <- SQL.eventById dbConnection eventId
  case maybeEvent of
    Nothing      ->
      return $ Left $ err404
    Just existingEvent -> do
      createdBy <- fetchProfileForEvent dbConnection existingEvent
      (validationMessages, validatedEventRequest) <-
        validateEvent dbConnection existingEvent eventRequest
      if not (Prelude.null validationMessages)
        then do
          let
            responseBody =
              EventConverter.responseForBadRequest
                existingEvent
                validationMessages
                validatedEventRequest
          return $ Left $
            err400 { errBody = JSON.encode responseBody }
        else do
          let
            eventUpdate =
              EventConverter.requestToModel
                existingEvent validatedEventRequest
          SQL.updateEvent dbConnection eventId eventUpdate
          -- TODO Handle Nothing
          Just updatedEvent <- SQL.eventById dbConnection eventId
          return $
            Right $ EventConverter.modelToResponse updatedEvent createdBy


deleteEvent ::
  IConnection connection =>
  DbConnection connection
  -> UUID
  -> ExceptT ServantErr IO NoContent
deleteEvent dbConnection eventId = do
  result <- liftIO $ withTransaction dbConnection $ \transactedConnection ->
    deleteEventIO
      transactedConnection
      eventId
  case result of
    Left  err -> throwE err
    Right _   -> return NoContent


deleteEventIO ::
  DbConnection connection
  -> UUID
  -> IO (Either ServantErr ())
deleteEventIO dbConnection eventId = do
  maybeEvent <- SQL.eventById dbConnection eventId
  case maybeEvent of
    Nothing ->
      return $ Left err404
    Just _ -> do
      SQL.deleteEvent dbConnection eventId
      return $ Right ()


fetchProfileForEvent :: DbConnection connection -> Event -> IO (Maybe Profile)
fetchProfileForEvent dbConnection event = do
  let createdById = Event.createdBy event
  createdByProfile <-
    case createdById of
      Just createdBy -> Profile.SQL.profileById dbConnection createdBy
      Nothing        -> return Nothing
  return createdByProfile


validateEvent ::
  DbConnection connection
  -> Event
  -> EventRequest
  -> IO ([Text], EventRequest)
validateEvent dbConnection _ {- existingEvent -} request = do
  let
    checks = []
  (messages, validatedRequest) <-
    Validation.validate dbConnection checks request
  return (messages, validatedRequest)

