{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Esqueleto.Internal.JSON
    where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Internal.Internal
import Database.Persist

class SqlToJson jsonValue a b | jsonValue a -> b where
    toJson :: a -> SqlExpr (jsonValue b)

class JsonAgg jsonValue where
    jsonAgg :: SqlExpr (jsonValue a) -> SqlExpr (jsonValue [a])

class JsonBuildArray jsonValue where
    unsafeJsonbBuildArray :: UnsafeSqlFunctionArgument a => a -> SqlExpr (jsonValue b)

class JsonBuildObject jsonValue where
    unsafeJsonbBuildObject :: [(SqlExpr (Value Text), SqlExpr SomeValue)] -> SqlExpr (jsonValue a)

multiset :: forall jsonValue a b r.
            ( Aeson.FromJSON b
            , SqlToJson jsonValue a b
            , JsonAgg jsonValue
            , SqlSelect (SqlExpr (jsonValue [b])) r
            )
         => SqlQuery a -> SqlExpr (jsonValue [b])
multiset q =
    subSelectUnsafe $ jsonAgg . toJson <$> q

instance SqlToJson jsonValue (SqlExpr a) b
      => SqlToJson jsonValue (SqlExpr (Maybe a)) (Maybe b) where
    toJson =
        let unMaybe :: SqlExpr (Maybe a) -> SqlExpr a
            unMaybe = veryVeryUnsafeCoerceSqlExpr
        in veryVeryUnsafeCoerceSqlExpr . toJson @jsonValue . unMaybe


instance forall a jsonValue . (PersistEntity a, JsonBuildObject jsonValue)
  => SqlToJson jsonValue (SqlExpr (Entity a)) (Entity a) where
    toJson ent =
        unsafeJsonbBuildObject fields
        where
            ed = entityDef $ Proxy @a
            baseFields = fmap (\fieldDef ->
                ( unsafeSqlValue $ TLB.fromText $ "'" <> unFieldNameHS (fieldHaskell fieldDef) <> "'"
                , ERaw noMeta $ \_ info -> (viewFieldBuilder ent info fieldDef, [])
                )) (getEntityFields ed)
            idField = fmap (\fieldDef ->
                ( unsafeSqlValue "'id'"
                , ERaw noMeta $ \_ info -> (viewFieldBuilder ent info fieldDef, [])
                )) (getEntityIdField ed)

            fields = maybe baseFields (:baseFields) idField


instance ( SqlToJson jsonValue a a'
         , SqlToJson jsonValue b b'
         , JsonBuildArray jsonValue
         )
  => SqlToJson jsonValue (a, b) (a', b') where
    toJson (a, b) =
        unsafeJsonbBuildArray
            ( toJson @jsonValue a
            , toJson @jsonValue b
            )

instance ( SqlToJson jsonValue a a'
         , SqlToJson jsonValue b b'
         , SqlToJson jsonValue c c'
         , JsonBuildArray jsonValue
         )
  => SqlToJson jsonValue (a, b, c) (a', b', c') where
    toJson (a, b, c) =
        unsafeJsonbBuildArray
            ( toJson @jsonValue a
            , toJson @jsonValue b
            , toJson @jsonValue c
            )

instance ( SqlToJson jsonValue a a'
         , SqlToJson jsonValue b b'
         , SqlToJson jsonValue c c'
         , SqlToJson jsonValue d d'
         , JsonBuildArray jsonValue
         )
  => SqlToJson jsonValue (a, b, c, d) (a', b', c', d') where
    toJson (a, b, c, d) =
        unsafeJsonbBuildArray
            ( toJson @jsonValue a
            , toJson @jsonValue b
            , toJson @jsonValue c
            , toJson @jsonValue d
            )
instance ( SqlToJson jsonValue a a'
         , SqlToJson jsonValue b b'
         , SqlToJson jsonValue c c'
         , SqlToJson jsonValue d d'
         , SqlToJson jsonValue e e'
         , JsonBuildArray jsonValue
         )
  => SqlToJson jsonValue (a, b, c, d, e) (a', b', c', d', e') where
    toJson (a, b, c, d, e) =
        unsafeJsonbBuildArray
            ( toJson @jsonValue a
            , toJson @jsonValue b
            , toJson @jsonValue c
            , toJson @jsonValue d
            , toJson @jsonValue e
            )
instance ( SqlToJson jsonValue a a'
         , SqlToJson jsonValue b b'
         , SqlToJson jsonValue c c'
         , SqlToJson jsonValue d d'
         , SqlToJson jsonValue e e'
         , SqlToJson jsonValue f f'
         , JsonBuildArray jsonValue
         )
  => SqlToJson jsonValue (a, b, c, d, e, f) (a', b', c', d', e', f') where
    toJson (a, b, c, d, e, f) =
        unsafeJsonbBuildArray
            ( toJson @jsonValue a
            , toJson @jsonValue b
            , toJson @jsonValue c
            , toJson @jsonValue d
            , toJson @jsonValue e
            , toJson @jsonValue f
            )
instance ( SqlToJson jsonValue a a'
         , SqlToJson jsonValue b b'
         , SqlToJson jsonValue c c'
         , SqlToJson jsonValue d d'
         , SqlToJson jsonValue e e'
         , SqlToJson jsonValue f f'
         , SqlToJson jsonValue g g'
         , JsonBuildArray jsonValue
         )
  => SqlToJson jsonValue (a, b, c, d, e, f, g) (a', b', c', d', e', f', g') where
    toJson (a, b, c, d, e, f, g) =
        unsafeJsonbBuildArray
            ( toJson @jsonValue a
            , toJson @jsonValue b
            , toJson @jsonValue c
            , toJson @jsonValue d
            , toJson @jsonValue e
            , toJson @jsonValue f
            , toJson @jsonValue g
            )
instance ( SqlToJson jsonValue a a'
         , SqlToJson jsonValue b b'
         , SqlToJson jsonValue c c'
         , SqlToJson jsonValue d d'
         , SqlToJson jsonValue e e'
         , SqlToJson jsonValue f f'
         , SqlToJson jsonValue g g'
         , SqlToJson jsonValue h h'
         , JsonBuildArray jsonValue
         )
  => SqlToJson jsonValue (a, b, c, d, e, f, g, h) (a', b', c', d', e', f', g', h') where
    toJson (a, b, c, d, e, f, g, h) =
        unsafeJsonbBuildArray
            ( toJson @jsonValue a
            , toJson @jsonValue b
            , toJson @jsonValue c
            , toJson @jsonValue d
            , toJson @jsonValue e
            , toJson @jsonValue f
            , toJson @jsonValue g
            , toJson @jsonValue h
            )

sqlSelectProcessRowJSON :: (Applicative f, Aeson.FromJSON r)
                        => [PersistValue] -> Either Text (f r)
sqlSelectProcessRowJSON [PersistByteString bs] =
    case Aeson.eitherDecode $ LBS.fromStrict bs of
        Right r -> Right $ pure r
        Left e -> Left $ Text.pack e
sqlSelectProcessRowJSON [PersistText t] =
    first (<> (" " <> t)) $ sqlSelectProcessRowJSON [PersistByteString (encodeUtf8 t)]

sqlSelectProcessRowJSON _ = Left "Expected ByteString but database returned unexpected type"

