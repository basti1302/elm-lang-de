{-# LANGUAGE OverloadedStrings #-}

module AccessToken.Util
  ( accessTokenToCookie
  , readAccessTokenFromCookieHeader
  , readAccessTokenFromRequest
  ) where

import qualified Util.Config           as Config

import           Control.Applicative
import qualified Data.Binary.Builder
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (DiffTime, NominalDiffTime,
                                        UTCTime (UTCTime))
import qualified Data.Time             as Time
import           Network.Wai
import qualified Web.Cookie            as Cookie


accessTokenCookieName :: String
accessTokenCookieName = "github-access-token"


accessTokenValidDuration :: NominalDiffTime
accessTokenValidDuration =
  -- let the access token be valid for 365 days
 365 {- days    -} *
  24 {- hours   -} *
  60 {- minutes -} *
  60 {- seconds -}


readAccessTokenFromRequest :: Request -> Maybe String
readAccessTokenFromRequest req =
  let
    headers = requestHeaders req
    maybeCookieByteString = (lookup "Cookie" headers) <|> (lookup "cookie" headers)
  in
    case maybeCookieByteString of
      Just cookieByteString ->
        (readAccessTokenFromCookies . Cookie.parseCookiesText) cookieByteString
      Nothing -> Nothing


readAccessTokenFromCookieHeader :: Maybe Text -> Maybe String
readAccessTokenFromCookieHeader maybeCookieText =
  case maybeCookieText of
    Just cookieText ->
      (readAccessTokenFromCookies .
       Cookie.parseCookiesText .
       BSC8.pack .
       T.unpack) cookieText
    Nothing -> Nothing


readAccessTokenFromCookies :: Cookie.CookiesText -> Maybe String
readAccessTokenFromCookies cookies =
  let
    accessTokenCookieValue = lookup (T.pack accessTokenCookieName) cookies
  in
    fmap T.unpack accessTokenCookieValue


accessTokenToCookie ::
  Config.WebConfig
  -> String
  -> IO String
accessTokenToCookie webConfig accessToken = do
  now <- Time.getCurrentTime
  let
    validUntil = Time.addUTCTime accessTokenValidDuration now
    cookie = Cookie.def
             { Cookie.setCookieName     = BSC8.pack accessTokenCookieName
             , Cookie.setCookieValue    = BSC8.pack accessToken
             , Cookie.setCookiePath     = Just "/"
             , Cookie.setCookieSecure   =
                 not $ Config.secureCookiesDisabled webConfig
             , Cookie.setCookieSameSite = Just Cookie.sameSiteStrict
             , Cookie.setCookieHttpOnly = True
             , Cookie.setCookieExpires  = Just validUntil
             , Cookie.setCookieMaxAge   = Just $ diffUTCTime validUntil now
             }
    cookieHeaderLazy = (Data.Binary.Builder.toLazyByteString .
                    Cookie.renderSetCookie)
                    cookie
  return $ BSC8.unpack $ Data.ByteString.Lazy.toStrict cookieHeaderLazy


diffUTCTime :: UTCTime -> UTCTime -> DiffTime
diffUTCTime (UTCTime daysA secondsA) (UTCTime daysB secondsB) =
   let
     differenceInDays :: Integer
     differenceInDays = (Time.diffDays daysA daysB)
     differenceInDaysConvertedToSeconds :: Integer
     differenceInDaysConvertedToSeconds = differenceInDays * 86400
     differenceInDaysConvertedToSecondsAsDiffTime :: DiffTime
     differenceInDaysConvertedToSecondsAsDiffTime =
       Time.secondsToDiffTime differenceInDaysConvertedToSeconds
     differenceInTimeOfDay :: DiffTime
     differenceInTimeOfDay = secondsB - secondsA
   in
     differenceInDaysConvertedToSecondsAsDiffTime + differenceInTimeOfDay



