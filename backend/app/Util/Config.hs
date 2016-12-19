{-# LANGUAGE DeriveGeneric #-}

module Util.Config
  ( AppConfig(..)
  , DbConfig(..)
  , EMailConfig(..)
  , readConfig
  , WebConfig(..)
  ) where

import qualified ElmLangDe.Util        as Util

import           Control.Monad
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Maybe            (fromMaybe, isNothing)
import           GHC.Generics
import           Servant.Client        (Scheme (..))
import           System.Environment    (lookupEnv)


data AppConfig = AppConfig
  { dbConfig    :: DbConfig
  , eMailConfig :: EMailConfig
  , webConfig   :: WebConfig
  } deriving (Eq, Show, Generic)


data DbConfig = DbConfig
  { dbHost     :: String
  , db         :: String
  , dbUser     :: String
  , dbPassword :: String
  } deriving (Eq, Show, Generic)


data EMailConfig = EMailConfig
  { mailScheme   :: Scheme
  , mailHost     :: String
  , mailPort     :: Int
  , mailPath     :: String
  , mailApiKey   :: ByteString
  , mailDisabled :: Bool
  } deriving (Eq, Show, Generic)


data WebConfig = WebConfig
  { secureCookiesDisabled :: Bool
  } deriving (Eq, Show, Generic)


readConfig :: IO AppConfig
readConfig = do
  dbCfg    <- readDbConfig
  putStrLn $ "DB CONFIG: " ++ show(dbCfg { dbPassword = "***" })
  eMailCfg <- readEMailConfig
  putStrLn $ "MAIL SERVICE CONFIG: " ++ show(eMailCfg { mailApiKey = pack("***") })
  webCfg   <- readWebConfig
  putStrLn $ "WEB CONFIG: " ++ show(webCfg)
  return AppConfig
         { dbConfig    = dbCfg
         , eMailConfig = eMailCfg
         , webConfig   = webCfg
         }


readDbConfig :: IO DbConfig
readDbConfig = do
  pghost      <- lookupEnvWithDefault "PGHOST" "localhost"
  pgdb        <- lookupEnvWithDefault "PGDB"   "elmlangde"
  pguser      <- lookupEnvWithDefault "PGUSER" "elmlangde"
  pgpass      <- lookupEnvWithDefault "PGPASS" "elmlangde"
  return DbConfig
         { dbHost     = pghost
         , db         = pgdb
         , dbUser     = pguser
         , dbPassword = pgpass
         }


readEMailConfig :: IO EMailConfig
readEMailConfig = do
  mailDisabledFlag   <- lookupFlag "MAIL_DISABLED"
  mailSchemeMabye    <- lookupEnv  "MAIL_SERVICE_HTTP_SCHEME"
  let maybeStringToScheme :: Maybe String -> Scheme
      maybeStringToScheme s =
          case s of
            Just "http" -> Http
            _           -> Https
      mailHttpScheme = maybeStringToScheme mailSchemeMabye
  mailHostString     <- lookupEnvWithDefault "MAIL_SERVICE_HTTP_HOST" "api.mailgun.net"
  mailPortMaybe      <- lookupEnv            "MAIL_SERVICE_HTTP_PORT"
  let mailPortInt    = fromMaybe 443 $ Util.readWithMaybeToMaybe mailPortMaybe
  mailPathString     <-
    lookupEnvWithDefault
      "MAIL_SERVICE_HTTP_PATH"
      "TODO: Create a new mailgun account for elm-lang.de"
  mailSecretMaybe    <- lookupEnv "MAIL_SERVICE_SECRET"
  when (not mailDisabledFlag && isNothing mailSecretMaybe) $
     error "No mail service api key (MAIL_SERVICE_SECRET) configured, exiting."
  let mailSecret     = fromMaybe "" mailSecretMaybe
  return EMailConfig
         { mailScheme   = mailHttpScheme
         , mailHost     = mailHostString
         , mailPort     = mailPortInt
         , mailPath     = mailPathString
         , mailApiKey   = pack mailSecret
         , mailDisabled = mailDisabledFlag
         }


readWebConfig :: IO WebConfig
readWebConfig = do
  secureCookiesDisabledFlag <- lookupFlag "SECURE_COOKIES_DISABLED"
  return WebConfig
         { secureCookiesDisabled = secureCookiesDisabledFlag
         }


lookupEnvWithDefault :: String -> String -> IO String
lookupEnvWithDefault key defaultValue = do
  maybeValue <- lookupEnv key
  return $ fromMaybe defaultValue maybeValue


lookupFlag :: String -> IO Bool
lookupFlag key = do
  maybeValue <- lookupEnv key
  return $ case maybeValue of
             Just _ -> True
             _      -> False

