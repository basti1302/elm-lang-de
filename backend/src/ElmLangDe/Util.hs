module ElmLangDe.Util
  ( createUuid
  , createUuidString
  , generateCacheBuster
  , hashPassword
  , hashPasswordBS
  , readWithMaybeToMaybe
  ) where

import           Crypto.PasswordStore  (makePassword)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.UUID             (UUID)
import qualified Data.UUID             as UUID
import           Data.UUID.V4          (nextRandom)


createUuid :: IO UUID
createUuid = nextRandom


createUuidString :: IO String
createUuidString = do
   uuid <- createUuid
   return $ UUID.toString $ uuid


generateCacheBuster :: IO String
generateCacheBuster = createUuidString


hashPasswordBS :: BS.ByteString -> IO (BS.ByteString)
hashPasswordBS password = do
  saltAndHash <- makePassword password 19
  return $ saltAndHash


hashPassword :: String -> IO (BS.ByteString)
hashPassword password =
  hashPasswordBS $ BSC.pack password


{-| Reads from the Read monad with the given key (which is a Maybe String) and
 - returns a Maybe value. When given key is Nothing, Nothing is returned.
 - Otherwise, if the key is Just a string, but no value for this key is present,
 - Nothing is returned. If the key is just a String and is present, its value
 - is returned.
 -}
readWithMaybeToMaybe :: Read a => Maybe String -> Maybe a
readWithMaybeToMaybe ms =
  case ms of
    Just s  -> readMaybe s
    Nothing -> Nothing


{-| Reads from the Read monad with the given key and returns a Maybe
 - value. When given key is not present, at all, Nothing is returned, otherwise
 - Just the value.
 -}
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(val, "")] -> Just val
                _           -> Nothing

