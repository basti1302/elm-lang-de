{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Validation
  ( HasOptionalEMail(..)
  , HasOptionalURL(..)
  , validate
  , validateEMailEmptyOrWellformed
  , validateURLEmptyOrWellformed
  ) where

import           Database.StatementMap

import           Control.Monad         (foldM)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Maybe            as Maybe
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Network.URI           as URI
import qualified Text.Email.Validate   as EmailValidate (isValid)


-- |The 'validate' function takes a list of checks and executes them on anentity
-- value. Each check has the type
-- dbConnection -> entityType -> IO (Maybe Text, entityType)
-- that is, it takes a db connection and the entity value and returns an IO
-- action that might produce an error message (Maybe Text) and potentially
-- updates the entity. Note: Some checks, especially checks for uniquenessor
-- existence need to access the database, that is why it is fed to all checks.
validate ::
  forall dbConnection entityType.
  dbConnection -- ^ the db connection that is fed to all checks
  -> [dbConnection -> entityType -> IO (Maybe Text, entityType)]
     -- ^ the list of checks to execute
  -> entityType -- ^ the entity value that will be validated
  -> IO ([Text], entityType) -- ^ a list of message and the updated entity
validate dbConnection checks entity = do
  let
    -- feed db connection to all checks
    -- checksWithDbConnection :: [entityType -> IO (Maybe Text, entityType)]
    checksWithDbConnection = map ($ dbConnection) checks

  -- execute checks
  validationResults :: ([Text], entityType) <-
    foldM executeOneValidationCheck ([], entity) checksWithDbConnection

  return validationResults


executeOneValidationCheck ::
  forall entityType.
  ([Text], entityType)
  -> (entityType -> IO (Maybe Text, entityType))
  -> IO ([Text], entityType)
executeOneValidationCheck (messages, entity) validationCheck = do
  let
    nextCheckAction :: IO (Maybe Text, entityType)
    nextCheckAction = validationCheck entity
  (maybeNextMessage, updatedEntity) :: (Maybe Text, entityType) <- nextCheckAction
  let
    updatedMessages =
      case maybeNextMessage of
        Nothing          -> messages
        Just nextMessage -> messages ++ [ nextMessage ]
  return (updatedMessages, updatedEntity)


class HasOptionalEMail a where
  getEMail :: a -> Maybe Text
  setEMail :: Maybe Text -> a -> a


validateEMailEmptyOrWellformed ::
  (HasOptionalEMail hasEMail) =>
  DbConnection connection
  -> hasEMail
  -> IO (Maybe Text, hasEMail)
validateEMailEmptyOrWellformed _ hasEMail =
  let
    maybeEMail = getEMail hasEMail
  in
    case maybeEMail of
      Nothing ->
        return (Nothing, hasEMail)
      Just email ->
        if T.null email
        then
          return (Nothing, (setEMail Nothing hasEMail))
        else if not (EmailValidate.isValid $ BSC8.pack $ T.unpack email)
        then
          return $
            ( Just "Das ist keine valide E-Mail-Adresse.", hasEMail)
        else
          return (Nothing, hasEMail)


class HasOptionalURL a where
  getURL :: a -> Maybe Text
  setURL :: Maybe Text -> a -> a


validateURLEmptyOrWellformed ::
  (HasOptionalURL hasURL) =>
  DbConnection connection
  -> hasURL
  -> IO (Maybe Text, hasURL)
validateURLEmptyOrWellformed _ hasURL =
  let
    maybeURL = getURL hasURL
  in
    case maybeURL of
      Nothing ->
        return (Nothing, hasURL)
      Just url ->
        if T.null url
        then
          return (Nothing, (setURL Nothing hasURL))
        else
          let
            (message, normalizedUrlText) = validateURLWellformed url
          in
            return $ (message, (setURL (Just normalizedUrlText) hasURL))


validateURLWellformed :: Text -> (Maybe Text, Text)
validateURLWellformed url =
  let
    possibleHttp = T.toLower $ T.take 7 url
    possibleHttps = T.toLower $ T.take 8 url
    normalizedUrl =
      if (possibleHttp /= "http://") && (possibleHttps /= "https://")
      then "http://" ++ (T.unpack url)
      else T.unpack url
    valid = Maybe.isJust $ URI.parseURI normalizedUrl
  in
    if valid
    then
      (Nothing, T.pack normalizedUrl)
    else
      (Just "Das ist keine valide URL.", T.pack normalizedUrl)

