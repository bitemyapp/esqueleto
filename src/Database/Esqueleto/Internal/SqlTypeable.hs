{-# LANGUAGE DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , TypeFamilies
 #-}
module Database.Esqueleto.Internal.SqlTypeable where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html (Html)
import Data.Fixed
import Data.Int
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (UTCTime, TimeOfDay, Day)
import qualified Data.Vector as V
import Data.Word
import Numeric.Natural (Natural)

import Database.Esqueleto.Internal.PersistentImport

{--
   SqlTypeable should have an instance for every PersistFieldSql instance. Ultimately it would be good to merge the classes. Contains a type level sqlType function and a SqlNullable associated type that is True iff null can inhabit the type
 --}
class PersistFieldSql t => SqlTypeable t where
  -- What is the underlying SqlType of the provided type t
  type SqlType' t :: SqlType
  -- Whether null can inhabit t
  type SqlNullable t :: Bool

instance (PersistFieldSql (Maybe t), SqlTypeable t, SqlNullable t ~ 'False) => SqlTypeable (Maybe t) where
  type SqlType' (Maybe t) = SqlType' t
  type SqlNullable (Maybe t) = 'True
instance (PersistFieldSql (Key t)) => SqlTypeable (Key t) where
  type SqlType' (Key t) = 'SqlInt64
  type SqlNullable (Key t) = 'False
instance {-# OVERLAPPABLE #-} SqlTypeable t => SqlTypeable [t] where
  type SqlType' [t] = 'SqlString
  type SqlNullable [t] = 'False
instance SqlTypeable [Char] where
  type SqlType' [Char] = 'SqlString
  type SqlNullable [Char] = 'False
instance SqlTypeable T.Text where
  type SqlType' T.Text = 'SqlString
  type SqlNullable T.Text = 'False
instance SqlTypeable TL.Text where
  type SqlType' TL.Text = 'SqlString
  type SqlNullable TL.Text = 'False
instance SqlTypeable B.ByteString where
  type SqlType' B.ByteString = 'SqlBlob
  type SqlNullable B.ByteString = 'False
instance SqlTypeable Html where
  type SqlType' Html = 'SqlString
  type SqlNullable Html = 'False
instance SqlTypeable Bool where
  type SqlType' Bool = 'SqlBool
  type SqlNullable Bool = 'False
instance SqlTypeable Int where
  type SqlType' Int = 'SqlInt64
  type SqlNullable Int = 'False
instance SqlTypeable Double where
  type SqlType' Double = 'SqlReal
  type SqlNullable Double = 'False
instance SqlTypeable Int8 where
  type SqlType' Int8 = 'SqlInt32
  type SqlNullable Int8 = 'False
instance SqlTypeable Int16 where
  type SqlType' Int16 = 'SqlInt32
  type SqlNullable Int16 = 'False
instance SqlTypeable Int32 where
  type SqlType' Int32 = 'SqlInt32
  type SqlNullable Int32 = 'False
instance SqlTypeable Int64 where
  type SqlType' Int64 = 'SqlInt64
  type SqlNullable Int64 = 'False
instance SqlTypeable Word where
  type SqlType' Word = 'SqlInt64
  type SqlNullable Word = 'False
instance SqlTypeable Word8 where
  type SqlType' Word8 = 'SqlInt32
  type SqlNullable Word8 = 'False
instance SqlTypeable Word16 where
  type SqlType' Word16 = 'SqlInt32
  type SqlNullable Word16 = 'False
instance SqlTypeable Word32 where
  type SqlType' Word32 = 'SqlInt64
  type SqlNullable Word32 = 'False
instance SqlTypeable Word64 where
  type SqlType' Word64 = 'SqlInt64
  type SqlNullable Word64 = 'False
instance SqlTypeable Day where
  type SqlType' Day = 'SqlDay
  type SqlNullable Day = 'False
instance SqlTypeable TimeOfDay where
  type SqlType' TimeOfDay = 'SqlTime
  type SqlNullable TimeOfDay = 'False
instance SqlTypeable UTCTime where
  type SqlType' UTCTime = 'SqlDayTime
  type SqlNullable UTCTime = 'False
instance SqlTypeable a => SqlTypeable (V.Vector a) where
  type SqlType' (V.Vector a) = 'SqlString
  type SqlNullable (V.Vector a) = 'False
instance (Ord a, SqlTypeable a) => SqlTypeable (S.Set a) where
  type SqlType' (S.Set a) = 'SqlString
  type SqlNullable (S.Set a) = 'False
instance (SqlTypeable a, SqlTypeable b) => SqlTypeable (a,b) where
  type SqlType' (a, b) = 'SqlString
  type SqlNullable (a, b) = 'False
instance SqlTypeable v => SqlTypeable (IM.IntMap v) where
  type SqlType' (IM.IntMap v) = 'SqlString
  type SqlNullable (IM.IntMap v) = 'False
instance SqlTypeable v => SqlTypeable (M.Map T.Text v) where
  type SqlType' (M.Map T.Text v) = 'SqlString
  type SqlNullable (M.Map T.Text v) = 'False
instance SqlTypeable PersistValue where
  type SqlType' PersistValue = 'SqlInt64 -- since PersistValue should only be used like this for keys, which in SQL are Int64
  type SqlNullable PersistValue = 'False
instance SqlTypeable Checkmark where
  type SqlType' Checkmark = 'SqlBool
  type SqlNullable Checkmark = 'False
{-- TODO: How do we implement SqlNumeric. Should we have a new type that mirrors the persistent SQL Type?
instance SqlTypeable Rational where
  type SqlType' Rational = 'SqlNumeric 32 20
  type SqlNullable Rational = 'False
  --}
instance SqlTypeable Natural where
  type SqlType' Natural = 'SqlInt64
  type SqlNullable Natural = 'False
-- An embedded Entity
instance (PersistField record, PersistEntity record) => SqlTypeable (Entity record) where
  type SqlType' (Entity record) = 'SqlString
  type SqlNullable (Entity record) = 'False
