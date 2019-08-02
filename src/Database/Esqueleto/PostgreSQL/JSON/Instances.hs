{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Esqueleto.PostgreSQL.JSON.Instances where

import Data.Aeson (FromJSON(..), ToJSON(..), encode, eitherDecodeStrict)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T (concat, pack)
import qualified Data.Text.Encoding as TE (decodeUtf8, encodeUtf8)
import Database.Esqueleto (Value, just, val)
import Database.Esqueleto.Internal.PersistentImport
import Database.Esqueleto.Internal.Sql (SqlExpr)
import GHC.Generics (Generic)


-- | Newtype wrapper around any type with a JSON representation.
--
-- @since 3.1.0
newtype JSONB a = JSONB { unJSONB :: a }
  deriving
    ( Generic
    , FromJSON
    , ToJSON
    , Eq
    , Foldable
    , Functor
    , Ord
    , Read
    , Show
    , Traversable
    )

-- | 'SqlExpr' of a NULL-able 'JSONB' value. Hence the 'Maybe'.
--
-- Note: NULL here is a PostgreSQL NULL, not a JSON 'null'
type JSONBExpr a = SqlExpr (Value (Maybe (JSONB a)))

-- | Convenience function to lift a regular value into
-- a 'JSONB' expression.
jsonbVal :: (FromJSON a, ToJSON a) => a -> JSONBExpr a
jsonbVal = just . val . JSONB

-- | Used with certain JSON operators.
--
-- This data type has 'Num' and 'IsString' instances
-- for ease of use by using integer and string literals.
--
-- >>> 3 :: JSONAccessor
-- JSONIndex 3
-- >>> -3 :: JSONAccessor
-- JSONIndex -3
--
-- >>> "name" :: JSONAccessor
-- JSONKey "name"
--
-- NOTE: DO NOT USE ANY OF THE 'Num' METHODS ON THIS TYPE!
data JSONAccessor = JSONIndex Int
                  | JSONKey Text
  deriving (Generic, Eq, Show)

-- | I repeat, DO NOT use any method other than 'fromInteger'!
instance Num JSONAccessor where
  fromInteger = JSONIndex . fromInteger
  negate (JSONIndex i) = JSONIndex $ negate i
  negate (JSONKey _) = error "Can not negate a JSONKey"
  (+) = numErr
  (-) = numErr
  (*) = numErr
  abs = numErr
  signum = numErr

numErr :: a
numErr = error "Do not use 'Num' methods on JSONAccessors"

instance IsString JSONAccessor where
  fromString = JSONKey . T.pack

-- | @since 3.1.0
instance (FromJSON a, ToJSON a) => PersistField (JSONB a) where
  toPersistValue = PersistDbSpecific . BSL.toStrict . encode . unJSONB
  fromPersistValue pVal = fmap JSONB $ case pVal of
      PersistByteString bs -> first (badParse $ TE.decodeUtf8 bs) $ eitherDecodeStrict bs
      PersistText t -> first (badParse t) $ eitherDecodeStrict (TE.encodeUtf8 t)
      x -> Left $ fromPersistValueError "string or bytea" x

-- | jsonb
--
-- @since 3.1.0
instance (FromJSON a, ToJSON a) => PersistFieldSql (JSONB a) where
  sqlType _ = SqlOther "JSONB"

badParse :: Text -> String -> Text
badParse t = fromPersistValueParseError t . T.pack

fromPersistValueError
  :: Text -- ^ Database type(s), should appear different from Haskell name, e.g. "integer" or "INT", not "Int".
  -> PersistValue -- ^ Incorrect value
  -> Text -- ^ Error message
fromPersistValueError databaseType received = T.concat
    [ "Failed to parse Haskell newtype `JSONB a`; "
    , "expected ", databaseType
    , " from database, but received: ", T.pack (show received)
    , ". Potential solution: Check that your database schema matches your Persistent model definitions."
    ]

fromPersistValueParseError
  :: Text -- ^ Received value
  -> Text -- ^ Additional error
  -> Text -- ^ Error message
fromPersistValueParseError received err = T.concat
    [ "Failed to parse Haskell type `JSONB a`, "
    , "but received ", received
    , " | with error: ", err
    ]
