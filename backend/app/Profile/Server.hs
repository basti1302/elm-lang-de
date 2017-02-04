{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Profile.Server
  ( profileServer
  , signUp
  ) where

import           Database.StatementMap
import           Profile.API                 (ProfileAPI)
import qualified Profile.Converter           as ProfileConverter
import           Profile.Model               (Profile)
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
import qualified Data.List                   as List
import           Data.Maybe                  (catMaybes, listToMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
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
  let
    profilesSorted = sortTeamMembersFirst profiles
  return $ map ProfileConverter.modelToHead profilesSorted


sortTeamMembersFirst :: [Profile] -> [Profile]
sortTeamMembersFirst profiles =
  let
    teamMembers = ["basti1302", "dennisreimann"]
    sortFn p1 p2 =
      let
        gh1 =  Profile.gitHubOAuthLogin p1
        gh2 =  Profile.gitHubOAuthLogin p2
      in
        if (elem gh1 teamMembers) && (elem gh2 teamMembers)
          then compare gh1 gh2
        else if (elem gh1 teamMembers) && (notElem gh2 teamMembers)
          then LT
        else if (notElem gh1 teamMembers) && (elem gh2 teamMembers)
          then GT
        -- keep current ordering for non-team-members, this works
        -- because sort algo is stable
        else EQ
  in
    List.sortBy sortFn profiles


getProfile ::
  IConnection connection =>
  DbConnection connection
  -> Text
  -> ExceptT ServantErr IO ProfileResponse
getProfile dbConnection profileUrlFragment = do
  result <- liftIO $
            withTransaction dbConnection $ \transactedConnection ->
              fetchOneProfile transactedConnection profileUrlFragment
  case result of
    Right profile -> return profile
    Left err      -> throwE err


fetchOneProfile ::
  DbConnection connection
  -> Text
  -> IO (Either ServantErr ProfileResponse)
fetchOneProfile dbConnection profileUrlFragment = do
  maybeProfile <- SQL.profileByUrlFragment dbConnection profileUrlFragment
  case maybeProfile of
    Just profile ->
      return $
        Right $ ProfileConverter.modelToResponse profile
    Nothing -> return $ Left err404


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
    Just existingProfile -> do
      (validationMessages, validatedProfileRequest) <-
        validateProfile dbConnection existingProfile profileRequest
      if not (Prelude.null validationMessages)
        then do
          let
            responseBody =
              ProfileConverter.responseForBadRequest
                existingProfile
                validationMessages
                validatedProfileRequest
          return $ Left $
            err400 { errBody = JSON.encode responseBody }
        else do
          let
            profileUpdate =
              ProfileConverter.requestToModel
                existingProfile validatedProfileRequest
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
  result <- liftIO $ withTransaction dbConnection $ \transactedConnection ->
    deleteProfileIO
      transactedConnection
      profileId
  case result of
    Left  err -> throwE err
    Right _   -> return NoContent


deleteProfileIO ::
  DbConnection connection
  -> UUID
  -> IO (Either ServantErr ())
deleteProfileIO dbConnection profileId = do
  maybeProfile <- SQL.profileById dbConnection profileId
  case maybeProfile of
    Nothing ->
      return $ Left err404
    Just _ -> do
      SQL.deleteProfile dbConnection profileId
      return $ Right ()


validateProfile ::
  DbConnection connection
  -> Profile
  -> ProfileRequest
  -> IO ([Text], ProfileRequest)
validateProfile dbConnection existingProfile request = do
  let
    checks =
      [ validateNamePresent
      , Validation.validateEMailEmptyOrWellformed
      , Validation.validateURLEmptyOrWellformed
      , (validateUrlFragmentUnique existingProfile)
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
  Profile
  -> DbConnection connection
  -> ProfileRequest
  -> IO (Maybe Text, ProfileRequest)
validateUrlFragmentUnique existingProfile dbConnection request = do
  -- TODO Produce proper validation message instead of only correcting the url
  -- fragment.
  let
    profileId = Profile.id existingProfile
    candidates :: [Maybe Text]
    candidates =
      [
      -- user provided url fragment
        ProfileRequest.urlFragment request
      -- use github id as default
      , ProfileRequest.gitHubUsername request
      -- use github oauth login as fallback
      , Just $ Profile.gitHubOAuthLogin existingProfile
      -- last resort: use profile uuid
      , Just $ T.pack $ UUID.toString profileId      ]
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


-- | Not accessible via HTTP POST, only used during GitHub OAuth sign up.
signUp ::
  IConnection connection =>
  DbConnection connection
  -> Profile
  -> IO ()
signUp dbConnection profile =
  liftIO $ withTransaction dbConnection $ \transactedConnection ->
    createProfile
      transactedConnection
      profile


createProfile ::
  DbConnection connection
  -> Profile
  -> IO ()
createProfile dbConnection profile = do
  SQL.newProfile dbConnection profile
  return ()

