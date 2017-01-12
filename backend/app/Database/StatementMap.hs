module Database.StatementMap
  ( DbConnection (..)
  , findMissingStatements
  , getStatement
  , StatementID (..)
  , StatementMap
  , withTransaction
  ) where

import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Database.HDBC           (IConnection)
import qualified Database.HDBC           as HDBC
import           Database.HDBC.Statement (Statement)


data DbConnection connection = DbConnection
  { conn  :: connection
  , stmts :: StatementMap
  }


type StatementMap = Map StatementID Statement


data StatementID =
    ProfileFetchById
  | ProfileFetchByGitHubLogin
  | ProfileFetchAll
  | ProfileDelete
  | ProfileInsert
  | ProfileUpdate
  | ProfileUrlFragmentUnique
  deriving (Bounded, Enum, Eq, Ord, Show)


allStatementIDs :: [StatementID]
allStatementIDs = [(minBound :: StatementID) ..]


getStatement ::
  StatementID
  -> DbConnection connection
  -> Statement
getStatement statementId dbConnection =
  let Just stmt = Map.lookup statementId (stmts dbConnection)
  in stmt


findMissingStatements :: StatementMap -> [StatementID]
findMissingStatements sqlStatements =
   let usedIDs = Map.keys sqlStatements
       hasID statementId = statementId `elem` usedIDs
   in filter (not . hasID) allStatementIDs


withTransaction ::
  IConnection connection =>
  DbConnection connection
  -> (DbConnection connection -> IO a)
  -> IO a
withTransaction dbConnection callback =
  let cnnctn = conn dbConnection
  in HDBC.withTransaction cnnctn $ \ transactedConnection ->
    callback dbConnection { conn = transactedConnection }

