{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Database.Migration
Description : Executes all outstanding database migrations at app startup

elm-lang-de uses the dbmigrations package to handle the evolution of the
database schema in a controlled manner. Migrations are added as text files
in backend/migrations. These migrations can be applied with the command line
tool moo-postgres (part of dbmigrations-postgresql). However, it is not
necessary to apply the migrations explicitly - rather, we to make sure that
the database schema is always up to date by applying all required migrations
at server startup. This is the responsibility of this module.
-}
module Database.Migration (upgradeDatabase) where

import           Control.Monad                           (forM_)
import           Database.HDBC
import           Database.Schema.Migrations
import           Database.Schema.Migrations.Backend
import           Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import           Database.Schema.Migrations.Filesystem   (FilesystemStoreSettings (..),
                                                          filesystemStore)
import           Database.Schema.Migrations.Store        (loadMigrations)
import qualified Moo.CommandUtils                        as MooUtils
import qualified Moo.Core                                as MooCore
import           System.Exit                             (exitFailure)


upgradeDatabase ::
  IConnection connection =>
  connection
  -> IO ()
upgradeDatabase connection = do
  dbMigrationsConfig <- MooCore.loadConfiguration Nothing
  case dbMigrationsConfig of
       Left err            -> putStrLn err >> exitFailure
       Right configuration -> upgradeWithConfig connection configuration


upgradeWithConfig ::
  IConnection connection =>
  connection
  -> MooCore.Configuration
  -> IO ()
upgradeWithConfig connection dbMigrationsConfig = do
  let
    migrationsPath :: FilePath = MooCore._migrationStorePath dbMigrationsConfig
    store = filesystemStore $ FSStore { storePath = migrationsPath }
    backend :: Backend = hdbcBackend connection
  loadedStoreData <- loadMigrations store
  case loadedStoreData of
       Left es -> do
         putStrLn "dbmigrations: There were errors in the migration store:"
         forM_ es $ \err -> putStrLn $ "  " ++ show err
         exitFailure
       Right storeData -> do
         ensureBootstrappedBackend backend >> commitBackend backend
         migrationNames <- missingMigrations backend storeData
         if (null migrationNames)
           then do
             putStrLn "dbmigrations: Database is up to date."
           else do
             forM_ migrationNames $ \migrationName -> do
                 m <- MooUtils.lookupMigration storeData migrationName
                 MooUtils.apply m storeData backend False
             commitBackend backend
             putStrLn "dbmigrations: Database successfully upgraded."

