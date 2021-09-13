{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Esqueleto.PostgreSQL.JSON.Experimental
    where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Experimental.ToMaybe
import Database.Esqueleto.Internal.Internal
import Database.Persist

newtype JsonValue a = JsonValue { unJsonValue :: a }
    deriving (Show, Eq)

instance (Aeson.FromJSON a)
  => SqlSelect (SqlExpr (JsonValue a)) (JsonValue a) where
    -- returns the list of 'PersistValue's that will be given to
    -- 'rawQuery'.
    sqlSelectCols info a = materializeExpr info a

    -- | Number of columns that will be consumed.
    sqlSelectColCount _ = 1

    -- | Transform a row of the result into the data type.
    sqlSelectProcessRow [PersistByteString bs] =
        case Aeson.decode $ LBS.fromStrict bs of
            Just r -> Right $ JsonValue r
            Nothing -> Left "Failed to decode"
    sqlSelectProcessRow _ = Left "Expected ByteString but database returned unexpected type"

instance ToMaybe (SqlExpr (JsonValue a)) where
    type ToMaybeT (SqlExpr (JsonValue a)) = SqlExpr (JsonValue (Maybe (Nullable a)))
    toMaybe = veryVeryUnsafeCoerceSqlExpr

instance ToAlias (SqlExpr (JsonValue a)) where
    toAlias e@(ERaw m f)
      | Just _ <- sqlExprMetaAlias m = pure e
      | otherwise = do
            ident <- newIdentFor (DBName "v")
            pure $ ERaw noMeta{sqlExprMetaAlias = Just ident} f
instance ToAliasReference (SqlExpr (JsonValue a)) where
    toAliasReference aliasSource (ERaw m _)
      | Just alias <- sqlExprMetaAlias m = pure $ ERaw m{sqlExprMetaIsReference = True} $ \_ info ->
          (useIdent info aliasSource <> "." <> useIdent info alias, [])
    toAliasReference _ e = pure e

jsonAgg :: SqlExpr (JsonValue a) -> SqlExpr (JsonValue [a])
jsonAgg v =
    veryVeryUnsafeCoerceSqlExpr $
            unsafeSqlFunction "coalesce"
            [ unsafeSqlFunction "jsonb_agg" $ veryVeryUnsafeCoerceSqlExpr v
            , ERaw noMeta $ \_ _ -> ("'[]'::jsonb", [])
            ]

multisetAgg :: (Aeson.FromJSON b, PgToJsonb (SqlExpr a) b) => SqlExpr a -> SqlExpr (JsonValue [b])
multisetAgg = jsonAgg . toJsonb

multiset :: (Aeson.FromJSON b, PgToJsonb a b) => SqlQuery a -> SqlExpr (JsonValue [b])
multiset q =
    subSelectUnsafe $ jsonAgg . toJsonb <$> q

class PgToJsonb a b | a -> b where
    toJsonb :: a -> SqlExpr (JsonValue b)

instance PgToJsonb (SqlExpr a) b => PgToJsonb (SqlExpr (Maybe a)) (Maybe b) where
    toJsonb = veryVeryUnsafeCoerceSqlExpr . toJsonb @(SqlExpr a) . veryVeryUnsafeCoerceSqlExpr

instance PgToJsonb (SqlExpr (JsonValue a)) a where
    toJsonb = id

instance PgToJsonb (SqlExpr (Value a)) a where
    toJsonb = veryVeryUnsafeCoerceSqlExpr . unsafeSqlFunction "to_jsonb"

instance forall a. PersistEntity a
  => PgToJsonb (SqlExpr (Entity a)) (Entity a) where
    toJsonb ent =
        unsafeJsonbBuildObject fields
        where
            ed = entityDef $ Proxy @a
            baseFields = fmap (\fieldDef ->
                ( "'" <> unFieldNameHS (fieldHaskell fieldDef) <> "'"
                , ERaw noMeta $ \_ info -> (viewFieldBuilder ent info fieldDef, [])
                )) (getEntityFields ed)
            idField = fmap (\fieldDef ->
                ( "'id'"
                , ERaw noMeta $ \_ info -> (viewFieldBuilder ent info fieldDef, [])
                )) (getEntityIdField ed)

            fields = maybe baseFields (:baseFields) idField

class TupleToSqlExpr a where
    tupleToSqlExpr :: IdentInfo -> a -> ([TLB.Builder], [PersistValue])

instance TupleToSqlExpr (SqlExpr a) where
    tupleToSqlExpr info (ERaw _ f) =
        let (t, v) = f Never info
        in ([t], v)

instance (TupleToSqlExpr a, TupleToSqlExpr b) => TupleToSqlExpr (a, b) where
    tupleToSqlExpr info (a, b) =
        tupleToSqlExpr info a <> tupleToSqlExpr info b

instance ( TupleToSqlExpr a
         , TupleToSqlExpr b
         , TupleToSqlExpr c
         ) => TupleToSqlExpr (a, b, c) where
    tupleToSqlExpr info =
        tupleToSqlExpr info . from3
instance ( TupleToSqlExpr a
         , TupleToSqlExpr b
         , TupleToSqlExpr c
         , TupleToSqlExpr d
         ) => TupleToSqlExpr (a, b, c, d) where
    tupleToSqlExpr info =
        tupleToSqlExpr info . from4

instance ( TupleToSqlExpr a
         , TupleToSqlExpr b
         , TupleToSqlExpr c
         , TupleToSqlExpr d
         , TupleToSqlExpr e
         ) => TupleToSqlExpr (a, b, c, d, e) where
    tupleToSqlExpr info =
        tupleToSqlExpr info . from5

instance ( TupleToSqlExpr a
         , TupleToSqlExpr b
         , TupleToSqlExpr c
         , TupleToSqlExpr d
         , TupleToSqlExpr e
         , TupleToSqlExpr f
         ) => TupleToSqlExpr (a, b, c, d, e, f) where
    tupleToSqlExpr info =
        tupleToSqlExpr info . from6

instance ( TupleToSqlExpr a
         , TupleToSqlExpr b
         , TupleToSqlExpr c
         , TupleToSqlExpr d
         , TupleToSqlExpr e
         , TupleToSqlExpr f
         , TupleToSqlExpr g
         ) => TupleToSqlExpr (a, b, c, d, e, f, g) where
    tupleToSqlExpr info =
        tupleToSqlExpr info . from7

instance ( TupleToSqlExpr a
         , TupleToSqlExpr b
         , TupleToSqlExpr c
         , TupleToSqlExpr d
         , TupleToSqlExpr e
         , TupleToSqlExpr f
         , TupleToSqlExpr g
         , TupleToSqlExpr h
         ) => TupleToSqlExpr (a, b, c, d, e, f, g, h) where
    tupleToSqlExpr info =
        tupleToSqlExpr info . from8


instance ( PgToJsonb a a'
         , PgToJsonb b b'
         )
  => PgToJsonb (a, b) (a', b') where
    toJsonb (a, b) =
        unsafeJsonbBuildArray $ flip tupleToSqlExpr
            ( toJsonb a
            , toJsonb b
            )

instance ( PgToJsonb a a'
         , PgToJsonb b b'
         , PgToJsonb c c'
         )
  => PgToJsonb (a, b, c) (a', b', c') where
    toJsonb (a, b, c) =
        unsafeJsonbBuildArray $ flip tupleToSqlExpr
            ( toJsonb a
            , toJsonb b
            , toJsonb c
            )

instance ( PgToJsonb a a'
         , PgToJsonb b b'
         , PgToJsonb c c'
         , PgToJsonb d d'
         )
  => PgToJsonb (a, b, c, d) (a', b', c', d') where
    toJsonb (a, b, c, d) =
        unsafeJsonbBuildArray $ flip tupleToSqlExpr
            ( toJsonb a
            , toJsonb b
            , toJsonb c
            , toJsonb d
            )
instance ( PgToJsonb a a'
         , PgToJsonb b b'
         , PgToJsonb c c'
         , PgToJsonb d d'
         , PgToJsonb e e'
         )
  => PgToJsonb (a, b, c, d, e) (a', b', c', d', e') where
    toJsonb (a, b, c, d, e) =
        unsafeJsonbBuildArray $ flip tupleToSqlExpr
            ( toJsonb a
            , toJsonb b
            , toJsonb c
            , toJsonb d
            , toJsonb e
            )
instance ( PgToJsonb a a'
         , PgToJsonb b b'
         , PgToJsonb c c'
         , PgToJsonb d d'
         , PgToJsonb e e'
         , PgToJsonb f f'
         )
  => PgToJsonb (a, b, c, d, e, f) (a', b', c', d', e', f') where
    toJsonb (a, b, c, d, e, f) =
        unsafeJsonbBuildArray $ flip tupleToSqlExpr
            ( toJsonb a
            , toJsonb b
            , toJsonb c
            , toJsonb d
            , toJsonb e
            , toJsonb f
            )
instance ( PgToJsonb a a'
         , PgToJsonb b b'
         , PgToJsonb c c'
         , PgToJsonb d d'
         , PgToJsonb e e'
         , PgToJsonb f f'
         , PgToJsonb g g'
         )
  => PgToJsonb (a, b, c, d, e, f, g) (a', b', c', d', e', f', g') where
    toJsonb (a, b, c, d, e, f, g) =
        unsafeJsonbBuildArray $ flip tupleToSqlExpr
            ( toJsonb a
            , toJsonb b
            , toJsonb c
            , toJsonb d
            , toJsonb e
            , toJsonb f
            , toJsonb g
            )
instance ( PgToJsonb a a'
         , PgToJsonb b b'
         , PgToJsonb c c'
         , PgToJsonb d d'
         , PgToJsonb e e'
         , PgToJsonb f f'
         , PgToJsonb g g'
         , PgToJsonb h h'
         )
  => PgToJsonb (a, b, c, d, e, f, g, h) (a', b', c', d', e', f', g', h') where
    toJsonb (a, b, c, d, e, f, g, h) =
        unsafeJsonbBuildArray $ flip tupleToSqlExpr
            ( toJsonb a
            , toJsonb b
            , toJsonb c
            , toJsonb d
            , toJsonb e
            , toJsonb f
            , toJsonb g
            , toJsonb h
            )

unsafeJsonbBuildArray :: (IdentInfo -> ([TLB.Builder], [PersistValue])) -> SqlExpr a
unsafeJsonbBuildArray f =
        ERaw noMeta $ \_ info ->
            let (t, v) = f info
            in ("jsonb_build_array(" <> uncommas t <> ")", v)

unsafeJsonbBuildObject :: [(Text, SqlExpr SomeValue)] -> SqlExpr (JsonValue a)
unsafeJsonbBuildObject fields =
    ERaw noMeta $ \p info ->
        let (texts, vals) = foldMap (\(haskellName, ERaw _ f) ->
                     let (t, v) = f p info
                     in ([TLB.fromText haskellName, t], v)) fields
        in ("jsonb_build_object(" <> uncommas texts <> ")", vals)
