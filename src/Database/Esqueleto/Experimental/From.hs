{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.From
    where

import qualified Control.Monad.Trans.Writer as W
import Data.Coerce (coerce)
import Data.Proxy
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)
import Database.Esqueleto.Internal.PersistentImport

-- | 'FROM' clause, used to bring entities into scope.
--
-- Internally, this function uses the `From` datatype.
-- Unlike the old `Database.Esqueleto.from`, this does not
-- take a function as a parameter, but rather a value that
-- represents a 'JOIN' tree constructed out of instances of `From`.
-- This implementation eliminates certain
-- types of runtime errors by preventing the construction of
-- invalid SQL (e.g. illegal nested-@from@).
from :: ToFrom a a' => a -> SqlQuery a'
from f = do
    (a, clause) <- unFrom (toFrom f)
    Q $ W.tell mempty{sdFromClause=[FromRaw $ clause]}
    pure a

type RawFn = NeedParens -> IdentInfo -> (TLB.Builder, [PersistValue])

-- | Data type defining the "From" language. This should not
-- constructed directly in application code.
--
-- A @From@ is a SqlQuery which returns a reference to the result of calling from
-- and a function that produces a portion of a FROM clause. This gets passed to
-- the FromRaw FromClause constructor directly when converting
-- from a @From@ to a @SqlQuery@ using @from@
--
-- @since 3.5.0.0
newtype From a = From
    { unFrom :: SqlQuery (a, RawFn)}


-- | A helper class primarily designed to allow using @SqlQuery@ directly in
-- a From expression. This is also useful for embedding a @SqlSetOperation@,
-- as well as supporting backwards compatibility for the
-- data constructor join tree used prior to /3.5.0.0/
--
-- @since 3.5.0.0
class ToFrom a r | a -> r where
    toFrom :: a -> From r
instance ToFrom (From a) a where
    toFrom = id

{-# DEPRECATED Table "@since 3.5.0.0 - use 'table' instead" #-}
data Table a = Table

instance PersistEntity ent => ToFrom (Table ent) (SqlExpr_ ValueContext (Entity ent)) where
    toFrom _ = table

-- | Bring a PersistEntity into scope from a table
--
-- @
-- select $ from $ table \@People
-- @
--
-- @since 3.5.0.0
table :: forall ent. PersistEntity ent => From (SqlExpr_ ValueContext (Entity ent))
table = From $ do
    let ed = entityDef (Proxy @ent)
    ident <- newIdentFor (coerce $ getEntityDBName ed)
    let entity = unsafeSqlEntity ident
    pure $ ( entity, const $ base ident ed )
      where
        base ident@(I identText) def info =
            let db = coerce $ getEntityDBName def
            in ( (fromDBName info (coerce db)) <>
                     if  db == identText
                     then mempty
                     else " AS " <> useIdent info ident
               , mempty
               )


{-# DEPRECATED SubQuery "/Since: 3.4.0.0/ - It is no longer necessary to tag 'SqlQuery' values with @SubQuery@" #-}
newtype SubQuery a = SubQuery a
instance (SqlSelectCols a, ToAlias a, ToAliasReference a a') => ToFrom (SubQuery (SqlQuery a)) a' where
    toFrom (SubQuery q) = selectQuery q
instance (SqlSelectCols a, ToAlias a, ToAliasReference a a') => ToFrom (SqlQuery a) a' where
    toFrom = selectQuery

-- | Select from a subquery, often used in conjuction with joins but can be
-- used without any joins. Because @SqlQuery@ has a @ToFrom@ instance you probably
-- dont need to use this function directly.
--
-- @
-- select $
--      p <- from $
--              selectQuery do
--              p <- from $ table \@Person
--              limit 5
--              orderBy [ asc p ^. PersonAge ]
--      ...
-- @
--
-- @since 3.5.0.0
selectQuery :: (SqlSelectCols a, ToAlias a, ToAliasReference a a')
            => SqlQuery a -> From a'
selectQuery subquery = From $ do
    -- We want to update the IdentState without writing the query to side data
    (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ subquery
    aliasedValue <- toAlias ret
    -- Make a fake query with the aliased results, this allows us to ensure that the query is only run once
    let aliasedQuery = Q $ W.WriterT $ pure (aliasedValue, sideData)
    -- Add the FromQuery that renders the subquery to our side data
    subqueryAlias <- newIdentFor (DBName "q")
    -- Pass the aliased results of the subquery to the outer query
    -- create aliased references from the outer query results (e.g value from subquery will be `subquery`.`value`),
    -- this is probably overkill as the aliases should already be unique but seems to be good practice.
    ref <- toAliasReference subqueryAlias aliasedValue

    pure (ref, \_ info ->
            let (queryText,queryVals) = toRawSql SELECT info aliasedQuery
            in
            ( (parens queryText) <> " AS " <> useIdent info subqueryAlias
            , queryVals
            )
         )
