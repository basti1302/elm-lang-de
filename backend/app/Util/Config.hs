{-# LANGUAGE DeriveGeneric #-}

module Util.Config
  ( AppConfig(..)
  , DbConfig(..)
  , hasGitHubOAuthConfig
  , readConfig
  , WebConfig(..)
  ) where

import           Control.Monad            (unless)
import           Data.Maybe               (fromMaybe, isJust, isNothing)
import           Data.String              (fromString)
import           GHC.Generics
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment       (lookupEnv)
import           Text.Read                (readMaybe)


data AppConfig = AppConfig
  { dbConfig  :: DbConfig
  , webConfig :: WebConfig
  } deriving (Eq, Show, Generic)


data DbConfig = DbConfig
  { dbHost     :: String
  , db         :: String
  , dbUser     :: String
  , dbPassword :: String
  } deriving (Eq, Show, Generic)


data WebConfig = WebConfig
  { bindHost               :: Warp.HostPreference
  , bindPort               :: Int
  , gitHubClientId         :: Maybe String
  , gitHubClientSecret     :: Maybe String
  , gitHubOAuthRedirectUrl :: String
  , secureCookiesDisabled  :: Bool
  , developmentMode        :: Bool
  } deriving (Eq, Show, Generic)


hasGitHubOAuthConfig :: WebConfig -> Bool
hasGitHubOAuthConfig webCfg =
  isJust (gitHubClientId webCfg) &&
  isJust (gitHubClientSecret webCfg)


readConfig :: IO AppConfig
readConfig = do
  dbCfg    <- readDbConfig
  putStrLn $ "DB CONFIG: " ++ show(dbCfg { dbPassword = "***" })
  webCfg   <- readWebConfig
  putStrLn $ "WEB CONFIG: " ++ show(webCfg)
  return AppConfig
         { dbConfig    = dbCfg
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


readWebConfig :: IO WebConfig
readWebConfig = do
  -- Default value for bind host is HostIPv4, which means
  -- "any IPv4 or IPv6 hostname, IPv4 preferred", see
  -- https://hackage.haskell.org/package/warp-3.2.9/docs/Network-Wai-Handler-Warp.html#t:HostPreference
  hostString                <- lookupEnvWithDefault    "HOST" "HostIPv4"
  port                      <- lookupEnvIntWithDefault "PORT" 8000
  ghClientId                <- lookupEnvOptional       "GITHUB_CLIENT_ID"
  ghClientSecret            <- lookupEnvOptional       "GITHUB_CLIENT_SECRET"
  ghRedirectUrl             <- lookupEnvWithDefault    "GITHUB_REDIRECT_URL"
                               "https://elm-lang.de/oauth/github"
  secureCookiesDisabledFlag <- lookupFlag              "SECURE_COOKIES_DISABLED"
  devMode                   <- lookupFlag              "DEVELOPMENT_MODE"
  let
    hostPreference :: Warp.HostPreference
    hostPreference = fromString hostString
    webCfg = WebConfig
             { bindHost               = hostPreference
             , bindPort               = port
             , gitHubClientId         = ghClientId
             , gitHubClientSecret     = ghClientSecret
             , gitHubOAuthRedirectUrl = ghRedirectUrl
             , secureCookiesDisabled  = secureCookiesDisabledFlag
             , developmentMode        = devMode
             }
  unless (hasGitHubOAuthConfig webCfg)
    ( putStrLn
      "WARNING: GitHub OAuth cliend ID and/or client secret are not \
      \configured, sign in via GitHub will not work."
    )
  if (developmentMode webCfg)
    then do
      putStrLn "WARNING: elm-lang.de is running in development mode."
      putStrLn "Static assets will be served from the /frontend folder."
    else do
      putStrLn "elm-lang.de is running in production mode."
      putStrLn "Static assets will be served from the /dist folder."
  return webCfg


lookupEnvWithDefault :: String -> String -> IO String
lookupEnvWithDefault key defaultValue = do
  maybeValue <- lookupEnv key
  return $ fromMaybe defaultValue maybeValue


lookupEnvIntWithDefault :: String -> Int -> IO Int
lookupEnvIntWithDefault key defaultValue = do
  maybeValue <- lookupEnv key
  case maybeValue of
    Just string -> do
      let
        parsed :: Maybe Int
        parsed = readMaybe string
      case parsed of
        Just integer ->
          return integer
        Nothing -> do
          _ <- error
            ("Error: You provided the string \"" ++ string ++
             "\" as the value for configuration option " ++ key ++
             " but this option requires an integer value and I could" ++
             " not convert \"" ++ string ++ "\" to an integer.")
          return defaultValue
    Nothing ->
      return defaultValue


lookupEnvOptional :: String -> IO (Maybe String)
lookupEnvOptional key = do
  maybeValue <- lookupEnv key
  if isNothing maybeValue
    then do
      putStrLn $ "Configuration " ++ key ++ " has not been set. Some " ++
                 "functionality will not be available."
      return maybeValue
    else do
      return maybeValue


lookupFlag :: String -> IO Bool
lookupFlag key = do
  maybeValue <- lookupEnv key
  return $ case maybeValue of
             Just _ -> True
             _      -> False

