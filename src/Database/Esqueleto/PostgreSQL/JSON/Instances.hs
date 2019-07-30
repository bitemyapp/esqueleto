{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Database.Esqueleto.PostgreSQL.JSON.Instances where

import Data.Aeson (encode, eitherDecodeStrict)
import qualified Data.Aeson as Aeson (Value)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import Data.Text (Text)
import qualified Data.Text as T (concat, pack)
import qualified Data.Text.Encoding as TE (decodeUtf8, encodeUtf8)
import Database.Esqueleto.Internal.PersistentImport

-- Mainly copied over from Database.Persist.Postgresql.JSON
-- Since we don't need anything else and adding another dependency
-- just for these two instances is a bit overkill.

-- | @since 3.1.0
instance PersistField Aeson.Value where
  toPersistValue = PersistDbSpecific . BSL.toStrict . encode
  fromPersistValue pVal = case pVal of
      PersistByteString bs -> fromLeft (badParse $ TE.decodeUtf8 bs) $ eitherDecodeStrict bs
      PersistText t -> fromLeft (badParse t) $ eitherDecodeStrict (TE.encodeUtf8 t)
      x -> Left $ fromPersistValueError "string or bytea" x

-- | jsonb - @since 3.1.0
instance PersistFieldSql Aeson.Value where
  sqlType _ = SqlOther "JSONB"

badParse :: Text -> String -> Text
badParse t = fromPersistValueParseError t . T.pack

fromLeft :: (a -> b) -> Either a x -> Either b x
fromLeft f (Left a) = Left $ f a
fromLeft _ (Right r) = Right r

fromPersistValueError
  :: Text -- ^ Database type(s), should appear different from Haskell name, e.g. "integer" or "INT", not "Int".
  -> PersistValue -- ^ Incorrect value
  -> Text -- ^ Error message
fromPersistValueError databaseType received = T.concat
    [ "Failed to parse Haskell type `Aeson.Value`; "
    , "expected ", databaseType
    , " from database, but received: ", T.pack (show received)
    , ". Potential solution: Check that your database schema matches your Persistent model definitions."
    ]

fromPersistValueParseError
  :: Text -- ^ Received value
  -> Text -- ^ Additional error
  -> Text -- ^ Error message
fromPersistValueParseError received err = T.concat
    [ "Failed to parse Haskell type `Aeson.Value`, "
    , "but received ", received
    , " | with error: ", err
    ]
