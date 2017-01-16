{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Profile.SQL
  ( allProfiles
  , checkUrlFragmentUnique
  , deleteProfile
  , newProfile
  , prepareStatements
  , profileByGitHubLogin
  , profileById
  , profileByUrlFragment
  , updateProfile
  ) where

import           Database.StatementMap
import           Database.UUIDConversion ()
import           Profile.Model           (Profile)
import qualified Profile.Model           as Profile

import           Data.Convertible.Base   (Convertible)
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.Text               (Text)
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
   fetchAll          <- prepareFetchAllProfiles       cn
   byId              <- prepareFetchById              cn
   byUrlFragment     <- prepareFetchByUrlFragment     cn
   byGitHubLogin     <- prepareFetchByGitHubLogin     cn
   insert            <- prepareInsert                 cn
   delete            <- prepareDelete                 cn
   update            <- prepareUpdate                 cn
   fragmentUrlUnique <- prepareCheckUrlFragmentUnique cn
   let
     statements = ( Map.insert ProfileFetchById          byId
                  $ Map.insert ProfileFetchByUrlFragment byUrlFragment
                  $ Map.insert ProfileFetchByGitHubLogin byGitHubLogin
                  $ Map.insert ProfileFetchAll           fetchAll
                  $ Map.insert ProfileDelete             delete
                  $ Map.insert ProfileInsert             insert
                  $ Map.insert ProfileUpdate             update
                  $ Map.insert ProfileUrlFragmentUnique  fragmentUrlUnique
                  $ (stmts dbConnection)
                  )
   return $ dbConnection { stmts = statements }


prepareInsert ::
  IConnection connection =>
  connection
  -> IO Statement
prepareInsert connection =
 prepare connection
   "INSERT INTO profiles ( \
   \ id,                   \
   \ name,                 \
   \ url_fragment,         \
   \ job,                  \
   \ bio,                  \
   \ available,            \
   \ zip_code,             \
   \ city,                 \
   \ country,              \
   \ email,                \
   \ homepage,             \
   \ signup_method,        \
   \ github_oauth_login,   \
   \ github_username,      \
   \ github_avatar_url,    \
   \ gravatar_id,          \
   \ twitter_handle,       \
   \ created_at            \
   \ ) values              \
   \ (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"


newProfile ::
  DbConnection connection
  -> Profile
  -> IO ()
newProfile dbConnection profile = do
  traceIO $ "newProfile " ++ show(profile)
  let
    params = [ toSql $ Profile.id               profile
             , toSql $ Profile.name             profile
             , toSql $ Profile.urlFragment      profile
             , toSql $ Profile.job              profile
             , toSql $ Profile.bio              profile
             , toSql $ Profile.available        profile
             , toSql $ Profile.zipCode          profile
             , toSql $ Profile.city             profile
             , toSql $ Profile.country          profile
             , toSql $ Profile.email            profile
             , toSql $ Profile.homepage         profile
             , toSql $ Profile.signUpMethod     profile
             , toSql $ Profile.gitHubOAuthLogin profile
             , toSql $ Profile.gitHubUsername   profile
             , toSql $ Profile.gitHubAvatarUrl  profile
             , toSql $ Profile.gravatarId       profile
             , toSql $ Profile.twitterHandle    profile
             , toSql $ Profile.createdAt        profile
             ]
    stmt = getStatement ProfileInsert dbConnection
  rowsInserted <- execute stmt params
  traceIO $ "newProfile inserted " ++ show(rowsInserted) ++ " row(s)."
  return ()


selectAll :: String
selectAll =
    "SELECT               \
    \ id,                 \
    \ name,               \
    \ url_fragment,       \
    \ job,                \
    \ bio,                \
    \ available,          \
    \ zip_code,           \
    \ city,               \
    \ country,            \
    \ email,              \
    \ homepage,           \
    \ signup_method,      \
    \ github_oauth_login, \
    \ github_username,    \
    \ github_avatar_url,  \
    \ gravatar_id,        \
    \ twitter_handle,     \
    \ created_at          \
    \ FROM profiles "


prepareFetchById ::
  IConnection connection =>
  connection
  -> IO Statement
prepareFetchById connection =
  prepare connection $ selectAll ++ " WHERE id = ?"


profileById ::
  DbConnection connection
  -> UUID
  -> IO (Maybe Profile)
profileById dbConnection profileId = do
  traceIO $ "profileById " ++ (show profileId)
  profileByColumn dbConnection ProfileFetchById profileId


prepareFetchByUrlFragment ::
  IConnection connection =>
  connection
  -> IO Statement
prepareFetchByUrlFragment connection =
  prepare connection $ selectAll ++ " WHERE url_fragment = ?"


profileByUrlFragment ::
  DbConnection connection
  -> Text
  -> IO (Maybe Profile)
profileByUrlFragment dbConnection urlFragment = do
  traceIO $ "profileByUrlFragment " ++ (show urlFragment)
  profileByColumn dbConnection ProfileFetchByUrlFragment urlFragment


prepareFetchByGitHubLogin ::
  IConnection connection =>
  connection
  -> IO Statement
prepareFetchByGitHubLogin connection =
  prepare connection $ selectAll ++ " WHERE github_oauth_login = ?"


profileByGitHubLogin ::
  DbConnection connection
  -> Text
  -> IO (Maybe Profile)
profileByGitHubLogin dbConnection gitHubLogin = do
  traceIO $ "profileByGitHubLogin " ++ (show gitHubLogin)
  profileByColumn dbConnection ProfileFetchByGitHubLogin gitHubLogin


profileByColumn ::
  Convertible sqlValue SqlValue =>
  DbConnection connection
  -> StatementID
  -> sqlValue
  -> IO (Maybe Profile)
profileByColumn dbConnection statementId key = do
  let
    stmt = getStatement statementId dbConnection
    params = [ toSql key ]
  _ <- execute stmt params
  row <- fetchRow stmt
  return $ Profile.fromRow row


prepareFetchAllProfiles ::
  IConnection connection =>
  connection
  -> IO Statement
prepareFetchAllProfiles connection =
  prepare connection $ selectAll ++ " ORDER BY created_at DESC"


allProfiles ::
  DbConnection connection
  -> IO [Profile]
allProfiles dbConnection = do
  traceIO $ "allProfiles"
  let
    stmt = getStatement ProfileFetchAll dbConnection
  _ <- execute stmt []
  rows <- fetchAllRows stmt
  let
    rowsWrappedAsJust = map Just rows
  return $ catMaybes $ map Profile.fromRow rowsWrappedAsJust


prepareUpdate ::
  IConnection connection =>
  connection
  -> IO Statement
prepareUpdate connection =
  prepare connection
    "UPDATE profiles SET    \
    \ name              = ?,\
    \ url_fragment      = ?,\
    \ job               = ?,\
    \ bio               = ?,\
    \ available         = ?,\
    \ zip_code          = ?,\
    \ city              = ?,\
    \ country           = ?,\
    \ email             = ?,\
    \ homepage          = ?,\
    \ github_username   = ?,\
    \ github_avatar_url = ?,\
    \ gravatar_id       = ?,\
    \ twitter_handle    = ? \
    \ WHERE id          = ?"


-- | Manual profile update from web form.
updateProfile ::
  DbConnection connection
  -> UUID
  -> Profile
  -> IO ()
updateProfile dbConnection profileId profile = do
  -- TODO Check if profile exists at all
  traceIO $ "updateProfile " ++ (show profileId) ++ " " ++ (show profile)
  let
    stmt = getStatement ProfileUpdate dbConnection
    params = [ toSql $ Profile.name            profile
             , toSql $ Profile.urlFragment     profile
             , toSql $ Profile.job             profile
             , toSql $ Profile.bio             profile
             , toSql $ Profile.available       profile
             , toSql $ Profile.zipCode         profile
             , toSql $ Profile.city            profile
             , toSql $ Profile.country         profile
             , toSql $ Profile.email           profile
             , toSql $ Profile.homepage        profile
             , toSql $ Profile.gitHubUsername  profile
             , toSql $ Profile.gitHubAvatarUrl profile
             , toSql $ Profile.gravatarId      profile
             , toSql $ Profile.twitterHandle   profile
             , toSql profileId
             ]
  rowsUpdated <- execute stmt params
  traceIO $ "updateProfile updated " ++ show(rowsUpdated) ++ " row(s)."
  return ()


prepareDelete ::
  IConnection connection =>
  connection
  -> IO Statement
prepareDelete connection =
  prepare connection "DELETE FROM profiles where id = ?"


deleteProfile ::
  DbConnection connection
  -> UUID
  -> IO ()
deleteProfile dbConnection profileId = do
  traceIO $ "deleteProfile " ++ (show profileId)
  let
    stmt  = getStatement ProfileDelete dbConnection
    params = [ toSql profileId ]
  count <- execute stmt params
  traceIO $ "deleteProfile deleted " ++ show(count) ++ " profile(s)."
  return ()


prepareCheckUrlFragmentUnique ::
  IConnection connection =>
  connection
  -> IO Statement
prepareCheckUrlFragmentUnique connection =
  prepare connection
    "SELECT id FROM profiles WHERE id <> ? AND url_fragment = ?"


-- | Returns True if and only if the given url fragment is not used in any
-- profile except for a profile with the the given id.
checkUrlFragmentUnique ::
  DbConnection connection
  -> UUID
  -> Text
  -> IO (Bool)
checkUrlFragmentUnique dbConnection profileId urlFragment = do
  traceIO $
    "checkUrlFragmentUnique " ++ (show profileId) ++ " " ++ (show urlFragment)
  let
    stmt = getStatement ProfileUrlFragmentUnique dbConnection
    params = [ toSql profileId, toSql urlFragment ]
  _ <- execute stmt params
  rows <- fetchAllRows stmt
  return $ null rows

