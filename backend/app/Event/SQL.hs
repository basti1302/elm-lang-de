{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Event.SQL
  ( allEvents
  , deleteEvent
  , newEvent
  , prepareStatements
  , eventById
  , updateEvent
  ) where

import           Database.StatementMap
import           Database.UUIDConversion ()
import           Event.Model             (Event)
import qualified Event.Model             as Event

import           Data.Convertible.Base   (Convertible)
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.UUID               (UUID)
import           Database.HDBC           (IConnection, SqlValue, execute,
                                          fetchAllRows, fetchRow, prepare,
                                          toSql)
import           Database.HDBC.Statement (Statement)
import           Debug.Trace             (traceIO)


prepareStatements ::
  IConnection connection =>
  DbConnection connection ->
  IO (DbConnection connection)
prepareStatements dbConnection = do
   let
     cn = conn dbConnection
   fetchAll          <- prepareFetchAllEvents       cn
   byId              <- prepareFetchById              cn
   insert            <- prepareInsert                 cn
   delete            <- prepareDelete                 cn
   update            <- prepareUpdate                 cn
   let
     statements = ( Map.insert EventFetchById          byId
                  $ Map.insert EventFetchAll           fetchAll
                  $ Map.insert EventDelete             delete
                  $ Map.insert EventInsert             insert
                  $ Map.insert EventUpdate             update
                  $ (stmts dbConnection)
                  )
   return $ dbConnection { stmts = statements }


prepareInsert ::
  IConnection connection =>
  connection
  -> IO Statement
prepareInsert connection =
 prepare connection
   "INSERT INTO events ( \
   \ id,                 \
   \ title,              \
   \ url,                \
   \ description,        \
   \ created_by,         \
   \ created_at          \
   \ ) values            \
   \ (?, ?, ?, ?, ?, ?)"


newEvent ::
  DbConnection connection
  -> Event
  -> IO ()
newEvent dbConnection event = do
  traceIO $ "newEvent " ++ show(event)
  let
    params = [ toSql $ Event.id          event
             , toSql $ Event.title       event
             , toSql $ Event.url         event
             , toSql $ Event.description event
             , toSql $ Event.createdBy   event
             , toSql $ Event.createdAt   event
             ]
    stmt = getStatement EventInsert dbConnection
  rowsInserted <- execute stmt params
  traceIO $ "newEvent inserted " ++ show(rowsInserted) ++ " row(s)."
  return ()


selectAll :: String
selectAll =
   "SELECT        \
   \ id,          \
   \ title,       \
   \ url,         \
   \ description, \
   \ created_by,  \
   \ created_at   \
   \ FROM events "


prepareFetchById ::
  IConnection connection =>
  connection
  -> IO Statement
prepareFetchById connection =
  prepare connection $ selectAll ++ " WHERE id = ?"


eventById ::
  DbConnection connection
  -> UUID
  -> IO (Maybe Event)
eventById dbConnection eventId = do
  traceIO $ "eventById " ++ (show eventId)
  eventByColumn dbConnection EventFetchById eventId


eventByColumn ::
  Convertible sqlValue SqlValue =>
  DbConnection connection
  -> StatementID
  -> sqlValue
  -> IO (Maybe Event)
eventByColumn dbConnection statementId key = do
  let
    stmt = getStatement statementId dbConnection
    params = [ toSql key ]
  _ <- execute stmt params
  row <- fetchRow stmt
  return $ Event.fromRow row


prepareFetchAllEvents ::
  IConnection connection =>
  connection
  -> IO Statement
prepareFetchAllEvents connection =
  prepare connection $ selectAll ++ " ORDER BY created_at DESC"


allEvents ::
  DbConnection connection
  -> IO [Event]
allEvents dbConnection = do
  traceIO $ "allEvents"
  let
    stmt = getStatement EventFetchAll dbConnection
  _ <- execute stmt []
  rows <- fetchAllRows stmt
  let
    rowsWrappedAsJust = map Just rows
  return $ catMaybes $ map Event.fromRow rowsWrappedAsJust


prepareUpdate ::
  IConnection connection =>
  connection
  -> IO Statement
prepareUpdate connection =
  prepare connection
    "UPDATE events SET \
    \ title       = ?, \
    \ url         = ?, \
    \ description = ?, \
    \ created_by  = ?, \
    \ created_at  = ?  \
    \ WHERE id    = ?"


-- | Event update from web form.
updateEvent ::
  DbConnection connection
  -> UUID
  -> Event
  -> IO ()
updateEvent dbConnection eventId event = do
  -- TODO Check if event exists at all
  traceIO $ "updateEvent " ++ (show eventId) ++ " " ++ (show event)
  let
    stmt = getStatement EventUpdate dbConnection
    params = [ toSql $ Event.title       event
             , toSql $ Event.url         event
             , toSql $ Event.description event
             , toSql $ Event.createdBy   event
             , toSql $ Event.createdAt   event
             , toSql eventId
             ]
  rowsUpdated <- execute stmt params
  traceIO $ "updateEvent updated " ++ show(rowsUpdated) ++ " row(s)."
  return ()


prepareDelete ::
  IConnection connection =>
  connection
  -> IO Statement
prepareDelete connection =
  prepare connection "DELETE FROM events where id = ?"


deleteEvent ::
  DbConnection connection
  -> UUID
  -> IO ()
deleteEvent dbConnection eventId = do
  traceIO $ "deleteEvent " ++ (show eventId)
  let
    stmt  = getStatement EventDelete dbConnection
    params = [ toSql eventId ]
  count <- execute stmt params
  traceIO $ "deleteEvent deleted " ++ show(count) ++ " event(s)."
  return ()

