module Profile.SQL
  ( allProfiles
  , checkUrlFragmentUnique
  , deleteProfile
  , newProfile
  , prepareStatements
  , profileById
  , updateProfile
  ) where

import           Database.StatementMap
import           Database.UUIDConversion ()
import           Profile.Model           (Profile)
import qualified Profile.Model           as Profile

import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.Text               (Text)
import           Data.UUID               (UUID)
import           Database.HDBC           (IConnection, execute, fetchAllRows,
                                          fetchRow, prepare, toSql)
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
   insert            <- prepareInsert                 cn
   delete            <- prepareDelete                 cn
   update            <- prepareUpdate                 cn
   fragmentUrlUnique <- prepareCheckUrlFragmentUnique cn
   let
     statements = ( Map.insert ProfileFetchById         byId
                  $ Map.insert ProfileFetchAll          fetchAll
                  $ Map.insert ProfileDelete            delete
                  $ Map.insert ProfileInsert            insert
                  $ Map.insert ProfileUpdate            update
                  $ Map.insert ProfileUrlFragmentUnique fragmentUrlUnique
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
   \ id,              \
   \ name,            \
   \ url_fragment,    \
   \ job,             \
   \ bio,             \
   \ available,       \
   \ zip_code,        \
   \ city,            \
   \ country,         \
   \ email,           \
   \ homepage,        \
   \ github_username, \
   \ twitter_handle,  \
   \ created_at       \
   \ ) values         \
   \ (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"


newProfile ::
  DbConnection connection
  -> Profile
  -> IO ()
newProfile dbConnection profile = do
  traceIO $ "newProfile " ++ show(profile)
  let
    params = [ toSql $ Profile.id              profile
             , toSql $ Profile.name            profile
             , toSql $ Profile.urlFragment     profile
             , toSql $ Profile.job             profile
             , toSql $ Profile.bio             profile
             , toSql $ Profile.available       profile
             , toSql $ Profile.zipCode         profile
             , toSql $ Profile.city            profile
             , toSql $ Profile.country         profile
             , toSql $ Profile.email           profile
             , toSql $ Profile.homepage        profile
             , toSql $ Profile.githubUsername  profile
             , toSql $ Profile.twitterHandle   profile
             , toSql $ Profile.createdAt       profile
             ]
    stmt = getStatement ProfileInsert dbConnection
  rowsInserted <- execute stmt params
  traceIO $ "newProfile inserted " ++ show(rowsInserted) ++ " row(s)."
  return ()


prepareFetchById ::
  IConnection connection =>
  connection
  -> IO Statement
prepareFetchById connection =
  prepare connection
    "SELECT            \
    \ id,              \
    \ name,            \
    \ url_fragment,    \
    \ job,             \
    \ bio,             \
    \ available,       \
    \ zip_code,        \
    \ city,            \
    \ country,         \
    \ email,           \
    \ homepage,        \
    \ github_username, \
    \ twitter_handle,  \
    \ created_at       \
    \ FROM profiles    \
    \ WHERE id = ?"


profileById ::
  DbConnection connection
  -> UUID
  -> IO (Maybe Profile)
profileById dbConnection profileId = do
  traceIO $ "profileById " ++ (show profileId)
  let
    stmt = getStatement ProfileFetchById dbConnection
    params = [ toSql profileId ]
  _ <- execute stmt params
  row <- fetchRow stmt
  return $ Profile.fromRow row


prepareFetchAllProfiles ::
  IConnection connection =>
  connection
  -> IO Statement
prepareFetchAllProfiles connection =
  prepare connection
    "SELECT            \
    \ id,              \
    \ name,            \
    \ url_fragment,    \
    \ job,             \
    \ bio,             \
    \ available,       \
    \ zip_code,        \
    \ city,            \
    \ country,         \
    \ email,           \
    \ homepage,        \
    \ github_username, \
    \ twitter_handle,  \
    \ created_at       \
    \ FROM profiles"


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
    "UPDATE profiles SET  \
    \ name            = ?,\
    \ url_fragment    = ?,\
    \ job             = ?,\
    \ bio             = ?,\
    \ available       = ?,\
    \ zip_code        = ?,\
    \ city            = ?,\
    \ country         = ?,\
    \ email           = ?,\
    \ homepage        = ?,\
    \ github_username = ?,\
    \ twitter_handle  = ?\
    \ WHERE id = ?"


-- TODO Check if profile exists at all
updateProfile ::
  DbConnection connection
  -> UUID
  -> Profile
  -> IO ()
updateProfile dbConnection profileId profile = do
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
             , toSql $ Profile.githubUsername  profile
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

