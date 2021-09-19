{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Esqueleto.PostgreSQL.JSON.Experimental
    where

import qualified Data.Aeson as Aeson
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Experimental.ToMaybe
import Database.Esqueleto.Internal.Internal
import qualified Database.Esqueleto.Internal.JSON as Internal

newtype JsonBValue a = JsonBValue { unJsonBValue :: a }
    deriving (Show, Eq)

instance Functor JsonBValue where
    fmap f = JsonBValue . f . unJsonBValue

instance Applicative JsonBValue where
    pure = JsonBValue
    (<*>) f v = JsonBValue $ unJsonBValue f $ unJsonBValue v

instance (Aeson.FromJSON a)
  => SqlSelect (SqlExpr (JsonBValue a)) (JsonBValue a) where
    sqlSelectCols info a = materializeExpr info a
    sqlSelectColCount _ = 1
    sqlSelectProcessRow = Internal.sqlSelectProcessRowJSON

instance ToMaybe (SqlExpr (JsonBValue a)) where
    type ToMaybeT (SqlExpr (JsonBValue a)) = SqlExpr (JsonBValue (Maybe (Nullable a)))
    toMaybe = veryVeryUnsafeCoerceSqlExpr

instance ToAlias (SqlExpr (JsonBValue a)) where
    toAlias e@(ERaw m f)
      | Just _ <- sqlExprMetaAlias m = pure e
      | otherwise = do
            ident <- newIdentFor (DBName "v")
            pure $ ERaw noMeta{sqlExprMetaAlias = Just ident} f
instance ToAliasReference (SqlExpr (JsonBValue a)) where
    toAliasReference aliasSource (ERaw m _)
      | Just alias <- sqlExprMetaAlias m = pure $ ERaw m{sqlExprMetaIsReference = True} $ \_ info ->
          (useIdent info aliasSource <> "." <> useIdent info alias, [])
    toAliasReference _ e = pure e

instance Internal.JsonBuildArray JsonBValue where
    unsafeJsonbBuildArray =
        unsafeSqlFunction "jsonb_build_array"

instance Internal.JsonBuildObject JsonBValue where
    unsafeJsonbBuildObject =
        unsafeSqlFunction "jsonb_build_object"

instance Internal.SqlToJson JsonBValue (SqlExpr (JsonBValue a)) a where
    toJson = id

instance Internal.SqlToJson JsonBValue (SqlExpr (Value a)) a where
    toJson = unsafeSqlFunction "to_jsonb"

instance Internal.JsonAgg JsonBValue where
    jsonAgg v =
        unsafeSqlFunction "coalesce"
            ( unsafeSqlFunction "jsonb_agg" v
            , unsafeSqlValue "'[]'::jsonb"
            )

-- Re-Exports with specified types
toJsonb :: Internal.SqlToJson JsonBValue a a' => a -> SqlExpr (JsonBValue a')
toJsonb = Internal.toJson

jsonbAgg :: SqlExpr (JsonBValue a) -> SqlExpr (JsonBValue [a])
jsonbAgg = Internal.jsonAgg

multiset :: (Internal.SqlToJson JsonBValue a a', Aeson.FromJSON a')
         => SqlQuery a -> SqlExpr (JsonBValue [a'])
multiset = Internal.multiset
