{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Profile.Server (profileServer) where

import           Database.StatementMap
import           ElmLangDe.Util              as Util
import           Profile.API                 (ProfileAPI)
import qualified Profile.Converter           as ProfileConverter
import qualified Profile.Model               as Profile
import           Profile.ProfileHeadResponse (ProfileHeadResponse)
import           Profile.Request             (ProfileRequest)
import qualified Profile.Request             as ProfileRequest
import           Profile.Response            (ProfileResponse)
import qualified Profile.SQL                 as SQL
import qualified Util.Validation             as Validation

import           Control.Monad               (mapM)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except
import qualified Data.Aeson                  as JSON (encode)
import           Data.Maybe                  (catMaybes, listToMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Time                   as Time
import           Data.UUID                   (UUID)
import qualified Data.UUID                   as UUID
import           Database.HDBC               (IConnection)
import           Servant


profileServer ::
  IConnection connection =>
  DbConnection connection
  -> Server ProfileAPI
profileServer dbConnection =
       getProfiles dbConnection
  :<|> getProfile dbConnection
  :<|> postProfile dbConnection
  :<|> putProfile dbConnection
  :<|> deleteProfile dbConnection


getProfiles ::
  IConnection connection =>
  DbConnection connection
  -> ExceptT ServantErr IO [ProfileHeadResponse]
getProfiles dbConnection =
  liftIO $
  withTransaction dbConnection $ \transactedConnection ->
    fetchAllProfiles transactedConnection


fetchAllProfiles ::
  DbConnection connection
  -> IO [ProfileHeadResponse]
fetchAllProfiles dbConnection = do
  profiles <- SQL.allProfiles dbConnection
  return $ map ProfileConverter.modelToHead profiles


getProfile ::
  IConnection connection =>
  DbConnection connection
  -> UUID
  -> ExceptT ServantErr IO ProfileResponse
getProfile dbConnection profileId = do
  result <- liftIO $
            withTransaction dbConnection $ \transactedConnection ->
              fetchOneProfile transactedConnection profileId
  case result of
    Right profile -> return profile
    Left err      -> throwE err


fetchOneProfile ::
  DbConnection connection
  -> UUID
  -> IO (Either ServantErr ProfileResponse)
fetchOneProfile dbConnection profileId = do
  maybeProfile <- SQL.profileById dbConnection profileId
  case maybeProfile of
    Just profile ->
      return $
        Right $ ProfileConverter.modelToResponse profile
    Nothing -> return $ Left err404


postProfile ::
  IConnection connection =>
  DbConnection connection
  -> ProfileRequest
  -> ExceptT ServantErr IO ProfileResponse
postProfile dbConnection profileRequest = do
  result <- liftIO $ withTransaction dbConnection $ \transactedConnection ->
    createProfile
      transactedConnection
      profileRequest
  case result of
    Left  err      -> throwE err
    Right response -> return response


createProfile ::
  DbConnection connection
  -> ProfileRequest
  -> IO (Either ServantErr ProfileResponse)
createProfile dbConnection profileRequest = do
  profileId <- createUuid
  createdAt <- Time.getCurrentTime
  (validationMessages, validatedProfileRequest) <-
    validateProfile dbConnection profileId profileRequest

  if not (Prelude.null validationMessages)
    then do
      let
        responseBody =
          ProfileConverter.responseForBadRequest
            profileId
            createdAt
            validationMessages
            validatedProfileRequest
      return $ Left $
        err400 { errBody = JSON.encode responseBody }
    else do
      let
        profile =
          ProfileConverter.requestToModel
            profileId createdAt validatedProfileRequest
      SQL.newProfile dbConnection profile
      return $ Right $
        ProfileConverter.modelToResponse
          profile


putProfile ::
  IConnection connection =>
  DbConnection connection
  -> UUID
  -> ProfileRequest
  -> ExceptT ServantErr IO ProfileResponse
putProfile dbConnection profileId profileRequest = do
  result <- liftIO $ withTransaction dbConnection $ \transactedConnection ->
    updateProfile
      transactedConnection
      profileId
      profileRequest
  case result of
    Left  err      -> throwE err
    Right response -> return response


updateProfile ::
  DbConnection connection
  -> UUID
  -> ProfileRequest
  -> IO (Either ServantErr ProfileResponse)
updateProfile dbConnection profileId profileRequest = do
  maybeProfile <- SQL.profileById dbConnection profileId
  case maybeProfile of
    Nothing      ->
      return $ Left $ err404
    Just currentProfileInDb -> do
      let
        createdAt = Profile.createdAt currentProfileInDb
      (validationMessages, validatedProfileRequest) <-
        validateProfile dbConnection profileId profileRequest

      if not (Prelude.null validationMessages)
        then do
          let
            responseBody =
              ProfileConverter.responseForBadRequest
                profileId
                createdAt
                validationMessages
                validatedProfileRequest
          return $ Left $
            err400 { errBody = JSON.encode responseBody }
        else do
          let
            profileUpdate =
              ProfileConverter.requestToModel
                profileId createdAt validatedProfileRequest
          SQL.updateProfile dbConnection profileId profileUpdate
          -- TODO Handle Nothing
          Just updatedProfile <- SQL.profileById dbConnection profileId
          return $
            Right $ ProfileConverter.modelToResponse updatedProfile


deleteProfile ::
  IConnection connection =>
  DbConnection connection
  -> UUID
  -> ExceptT ServantErr IO NoContent
deleteProfile dbConnection profileId = do
  liftIO $ withTransaction dbConnection $ \transactedConnection ->
    SQL.deleteProfile transactedConnection profileId
  return NoContent


validateProfile ::
  DbConnection connection
  -> UUID
  -> ProfileRequest
  -> IO ([Text], ProfileRequest)
validateProfile dbConnection profileId request = do
  let
    checks =
      [ validateNamePresent
      , Validation.validateEMailEmptyOrWellformed
      , Validation.validateURLEmptyOrWellformed
      , (validateUrlFragmentUnique profileId)
      ]
  (messages, validatedRequest) <-
    Validation.validate dbConnection checks request
  return (messages, validatedRequest)


validateNamePresent ::
  DbConnection connection
  -> ProfileRequest
  -> IO (Maybe Text, ProfileRequest)
validateNamePresent _ request = do
  let message = Just "Bitte gib deinen Namen an."
  case ProfileRequest.name request of
    Nothing ->
      return
        (message, request)
    Just name -> do
      let strippedName = T.strip name
      if (T.null strippedName)
      then
        return
          (message, request )
      else
        return (Nothing, request { ProfileRequest.name = Just strippedName })


validateUrlFragmentUnique ::
  UUID
  -> DbConnection connection
  -> ProfileRequest
  -> IO (Maybe Text, ProfileRequest)
validateUrlFragmentUnique profileId dbConnection request = do
  -- TODO Produce proper validation message instead of only correcting the url
  -- fragment.
  let
    candidates :: [Maybe Text]
    candidates =
      [ ProfileRequest.urlFragment    request   -- user provided url fragment
      , ProfileRequest.githubUsername request   -- use github un as default
      , Just $ T.pack $ UUID.toString profileId -- last resort: use profile id
      ]
    -- | Checks for a single candidate for the url fragment whether it is valid,
    -- that is, not empty and unique.
    checkCandidate :: Maybe Text -> IO (Maybe Text)
    checkCandidate candidate = do
      case candidate of
        Nothing -> return Nothing
        Just fragment ->
          if T.null fragment
            then return Nothing
            else do
              valid <-
                SQL.checkUrlFragmentUnique dbConnection profileId fragment
              if valid then return (Just fragment) else return Nothing

  checkedCandidates :: [Maybe Text] <- mapM checkCandidate candidates
  let
    firstValidCandidate = (listToMaybe . catMaybes) checkedCandidates
  case firstValidCandidate of
    Nothing ->
      return (Just "Diese URL kannst du nicht verwenden.", request)
    Just validFragment ->
      return
        (Nothing, request { ProfileRequest.urlFragment = Just validFragment })

