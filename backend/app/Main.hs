{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Main
Description : Application entry point of elm-lang-de backend

This is the main module of the elm-lang-de Haskell/Servant backend. It executes
some preparation tasks before starting the Servant based server.

This preparation tasks include:
* read the configuration parameters from the environment,
* open a connection to the database,
* prepare all SQL statements.
-}
module Main where

import qualified API
-- import qualified Auth                     (accessTokenAuthContext)
import qualified Database.Migration       as Migration
import           Database.StatementMap
import qualified Profile.SQL              (prepareStatements)
import qualified Server
import qualified Util.Config              as Config

import           Control.Exception        (bracket)
import           Control.Monad.Catch      (Handler)
import           Control.Retry
import qualified Data.Map.Strict          as Map
import           Data.Monoid              ((<>))
import           Database.HDBC            hiding (run)
import           Database.HDBC.PostgreSQL as PostgreSQL (Connection,
                                                         connectPostgreSQL)
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant


main :: IO ()
main = do
  appConfig <- Config.readConfig
  withPostgreSQL (Config.dbConfig appConfig)
    (initialize appConfig)


initialize ::
  IConnection connection =>
  Config.AppConfig
  -> connection
  -> IO ()
initialize appConfig connection = do
  putStrLn "Compiling prepared SQL statements..."
  dbConnection <- prepareStatements connection
  let
    missingStatementIDs = findMissingStatements $ stmts dbConnection
  if null missingStatementIDs
    then putStrLn "Prepared SQL statements have been compiled successfully."
    else error ("Missing StatementIDs in compiled prepared SQL statements: \n" ++
               (show missingStatementIDs) ++ "\n Exiting.")

  putStrLn "Upgrading database (dbmigrations)..."
  Migration.upgradeDatabase connection
  let
    webConfig = Config.webConfig appConfig
    host :: Warp.HostPreference
    host = Config.bindHost webConfig
    port = Config.bindPort webConfig
    serverSettings =
      Warp.setHost host $
      Warp.setPort port $
      Warp.defaultSettings
  putStrLn $ "Starting elm-lang.de backend on host " ++
             (show $ Warp.getHost serverSettings) ++ " and port " ++
             (show $ Warp.getPort serverSettings)
  Warp.runSettings serverSettings $ app appConfig dbConnection

withPostgreSQL ::
  Config.DbConfig
  -> (PostgreSQL.Connection -> IO a)
  -> IO a
withPostgreSQL dbConfig =
  bracket (connectToPostgres dbConfig) disconnect


-- Connects to PostgreSQL. When the connection fails, the connection attempt
-- will be retried every 0.5 seconds with 600 retries max (that is, it tries for
-- 5 minutes)
connectToPostgres ::
  Config.DbConfig
  -> IO PostgreSQL.Connection
connectToPostgres dbConfig =
  let
    -- delay is in microseconds/μs (millionth of a second,
    -- 1000 μs = 1 millisecond)
    delay = 500000 -- 1/2 second
    policy :: RetryPolicyM IO =
      constantDelay delay <> limitRetries 600
    reportSqlError :: Bool -> SqlError -> RetryStatus -> IO ()
    reportSqlError retriedOrCrashed err retryStatus = do
       putStrLn $ defaultLogMsg retriedOrCrashed err retryStatus
    retryOnAnySqlError :: SqlError -> IO Bool
    retryOnAnySqlError _ = return True
    retryOnAnyStatus :: RetryStatus -> Handler IO Bool
    retryOnAnyStatus = logRetries retryOnAnySqlError reportSqlError
    connectUrl = "host="      ++ (Config.dbHost     dbConfig) ++
                 " dbname="   ++ (Config.db         dbConfig) ++
                 " user="     ++ (Config.dbUser     dbConfig) ++
                 " password=" ++ (Config.dbPassword dbConfig)
    connect _ =
       PostgreSQL.connectPostgreSQL connectUrl
 in
    recovering
       policy
       [retryOnAnyStatus]
       connect


prepareStatements ::
  IConnection connection =>
  connection
  -> IO (DbConnection connection)
prepareStatements cn = do
  let
    dbConnection = DbConnection
                   { conn = cn
                   , stmts = Map.empty
                   }
  Profile.SQL.prepareStatements dbConnection
  -- add more prepareStatements here as required:
  -- >>= Foo.SQL.prepareStatements
  -- >>= Bar.SQL.prepareStatements
  >>= return


app ::
  IConnection connection =>
  Config.AppConfig
  -> DbConnection connection
  -> Wai.Application
app appConfig dbConnection =
  Servant.serve API.proxyApi $ Server.mainServer appConfig dbConnection
  -- TODO Implement sign up/sign in and auth
  -- serveWithContext API.proxyApi
  --   (Auth.accessTokenAuthContext dbConnection)
  --   (Server.mainServer appConfig dbConnection)

