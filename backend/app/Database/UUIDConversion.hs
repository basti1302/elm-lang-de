{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

{-|
This module handles automtic conversion from UUID values (Data.UUID.UUID) to and
from HDBC'S SqlValues.

The conversion to and from SqlValues is required, so we can store UUID values in
the database without manually converting them in each Model's fromRow method or
in the SQL modules when using toSql.

Model modules that use UUIDs should import this module with
import Database.UUIDConversion ()
to make sure the instances are available.
-}

module Database.UUIDConversion where


import qualified Data.ByteString.UTF8 as BUTF8 (toString)
import           Data.Convertible
import           Data.UUID            (UUID)
import qualified Data.UUID            as UUID
import           Data.UUID.Aeson      ()
import           Database.HDBC        (SqlValue (SqlByteString, SqlString))


instance Convertible SqlValue UUID where
    safeConvert (SqlString a) = case UUID.fromString a of
        Just b -> Right b
        Nothing -> Left $ ConvertError (show a) "SqlValue" "UUID" "Could not parse UUID"
    safeConvert (SqlByteString a) = safeConvert $ SqlString $ BUTF8.toString a
    safeConvert a = Left $ ConvertError (show a) "SqlValue" "UUID" "No conversion available"


instance Convertible UUID SqlValue where
    safeConvert = Right . SqlString . UUID.toString
