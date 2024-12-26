{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.From.SqlSetOperation
        where

import Control.Arrow (first)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Writer as W
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Experimental.From
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)
import Database.Esqueleto.Internal.PersistentImport (PersistValue)

-- | Data type used to implement the SqlSetOperation language
-- this type is implemented in the same way as a @From@
--
-- Semantically a @SqlSetOperation@ is always a @From@ but not vice versa
--
-- @since 3.5.0.0
newtype SqlSetOperation a = SqlSetOperation
    { unSqlSetOperation :: NeedParens -> SqlQuery (a, IdentInfo -> (TLB.Builder, [PersistValue]))}

instance ToAliasReference a => ToFrom (SqlSetOperation a) a where
    toFrom setOperation = From $ do
        ident <- newIdentFor (DBName "u")
        (a, fromClause) <- unSqlSetOperation setOperation Never
        ref <- toAliasReference ident a
        pure (ref, \_ info -> (first parens $ fromClause info) <> (" AS " <> useIdent info ident, mempty))

-- | Type class to support direct use of @SqlQuery@ in a set operation tree
--
-- @since 3.5.0.0
class ToSqlSetOperation a r | a -> r where
    toSqlSetOperation :: a -> SqlSetOperation r
instance ToSqlSetOperation (SqlSetOperation a) a where
    toSqlSetOperation = id
instance (SqlSelect a r, ToAlias a, ToAliasReference a) => ToSqlSetOperation (SqlQuery a) a where
    toSqlSetOperation subquery =
        SqlSetOperation $ \p -> do
            (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ subquery
            aliasedValue <- toAlias ret
            let aliasedQuery = Q $ W.WriterT $ pure (aliasedValue, sideData)
            let p' =
                  case p of
                    Parens -> Parens
                    Never ->
                      if (sdLimitClause sideData) /= mempty
                          || length (sdOrderByClause sideData) > 0 then
                        Parens
                      else
                        Never
            pure (aliasedValue, \info -> first (parensM p') $ toRawSql SELECT info aliasedQuery)

-- | Helper function for defining set operations
-- @since 3.5.0.0
mkSetOperation :: (ToSqlSetOperation a a', ToSqlSetOperation b a')
               => TLB.Builder -> a -> b -> SqlSetOperation a'
mkSetOperation operation lhs rhs = SqlSetOperation $ \p -> do
    state <- Q $ lift S.get
    (leftValue, leftClause) <- unSqlSetOperation (toSqlSetOperation lhs) p
    Q $ lift $ S.put state
    (_, rightClause) <- unSqlSetOperation (toSqlSetOperation rhs) p
    pure (leftValue, \info -> leftClause info <> (operation, mempty) <> rightClause info)

{-# DEPRECATED Union "/Since: 3.4.0.0/ - Use the 'union_' function instead of the 'Union' data constructor" #-}
data Union a b = a `Union` b
instance ToSqlSetOperation a a' => ToSqlSetOperation (Union a a) a' where
    toSqlSetOperation (Union a b) = union_ a b

-- | Overloaded @union_@ function to support use in both 'SqlSetOperation'
-- and 'withRecursive'
--
-- @since 3.5.0.0
class Union_ a where
    -- | @UNION@ SQL set operation. Can be used as an infix function between 'SqlQuery' values.
    union_ :: a

instance (ToSqlSetOperation a c, ToSqlSetOperation b c, res ~ SqlSetOperation c)
  => Union_ (a -> b -> res) where
    union_ = mkSetOperation " UNION "

-- | Overloaded @unionAll_@ function to support use in both 'SqlSetOperation'
-- and 'withRecursive'
--
-- @since 3.5.0.0
class UnionAll_ a where
    -- | @UNION@ @ALL@ SQL set operation. Can be used as an infix function between 'SqlQuery' values.
    unionAll_ :: a
instance (ToSqlSetOperation a c, ToSqlSetOperation b c, res ~ SqlSetOperation c)
  => UnionAll_ (a -> b -> res) where
    unionAll_ = mkSetOperation " UNION ALL "

{-# DEPRECATED UnionAll "/Since: 3.4.0.0/ - Use the 'unionAll_' function instead of the 'UnionAll' data constructor" #-}
data UnionAll a b = a `UnionAll` b
instance ToSqlSetOperation a a' => ToSqlSetOperation (UnionAll a a) a' where
    toSqlSetOperation (UnionAll a b) = unionAll_ a b

{-# DEPRECATED Except "/Since: 3.4.0.0/ - Use the 'except_' function instead of the 'Except' data constructor" #-}
data Except a b = a `Except` b
instance ToSqlSetOperation a a' => ToSqlSetOperation (Except a a) a' where
    toSqlSetOperation (Except a b) = except_ a b

-- | @EXCEPT@ SQL set operation. Can be used as an infix function between 'SqlQuery' values.
except_ :: (ToSqlSetOperation a a', ToSqlSetOperation b a') => a -> b -> SqlSetOperation a'
except_ = mkSetOperation " EXCEPT "

{-# DEPRECATED Intersect "/Since: 3.4.0.0/ - Use the 'intersect_' function instead of the 'Intersect' data constructor" #-}
data Intersect a b = a `Intersect` b
instance ToSqlSetOperation a a' => ToSqlSetOperation (Intersect a a) a' where
    toSqlSetOperation (Intersect a b) = intersect_ a b

-- | @INTERSECT@ SQL set operation. Can be used as an infix function between 'SqlQuery' values.
intersect_ :: (ToSqlSetOperation a a', ToSqlSetOperation b a') => a -> b -> SqlSetOperation a'
intersect_ = mkSetOperation " INTERSECT "

{-# DEPRECATED SelectQuery "/Since: 3.4.0.0/ - It is no longer necessary to tag 'SqlQuery' values with @SelectQuery@" #-}
pattern SelectQuery :: p -> p
pattern SelectQuery a = a

