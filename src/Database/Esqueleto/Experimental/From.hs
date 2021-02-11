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

import Control.Arrow (first)
import Control.Monad (ap)
import qualified Control.Monad.Trans.Writer as W
import Data.Proxy
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)
import Database.Esqueleto.Internal.PersistentImport

-- | 'FROM' clause, used to bring entities into scope.
--
-- Internally, this function uses the `From` datatype and the
-- `From` typeclass. Unlike the old `Database.Esqueleto.from`,
-- this does not take a function as a parameter, but rather
-- a value that represents a 'JOIN' tree constructed out of
-- instances of `From`. This implementation eliminates certain
-- types of runtime errors by preventing the construction of
-- invalid SQL (e.g. illegal nested-@from@).
from :: ToFrom a a' => a -> SqlQuery a'
from f = do
    (a, clause) <- unFrom (toFrom f)
    Q $ W.tell mempty{sdFromClause=[FromRaw $ clause]}
    pure a

type RawFn = NeedParens -> IdentInfo -> (TLB.Builder, [PersistValue])
newtype From a = From
    { unFrom :: SqlQuery (a, RawFn)}

class ToFrom a r | a -> r where
    toFrom :: a -> From r
instance ToFrom (From a) a where
    toFrom = id

-- | Data type for bringing a Table into scope in a JOIN tree
--
-- @
-- select $ from $ Table \@People
-- @
data Table a = Table
instance PersistEntity ent => ToFrom (Table ent) (SqlExpr (Entity ent)) where
    toFrom _ = table

table :: forall ent. PersistEntity ent => From (SqlExpr (Entity ent))
table = From $ do
    let ed = entityDef (Proxy @ent)
    ident <- newIdentFor (entityDB ed)
    let entity = unsafeSqlEntity ident
    pure $ ( entity, const $ base ident ed )
      where
        base ident@(I identText) def info =
            let db@(DBName dbText) = entityDB def
            in ( fromDBName info db <>
                     if dbText == identText
                     then mempty
                     else " AS " <> useIdent info ident
               , mempty
               )


{-# DEPRECATED SubQuery "/Since: 3.4.0.0/ - It is no longer necessary to tag 'SqlQuery' values with @SubQuery@" #-}
newtype SubQuery a = SubQuery a
instance (SqlSelect a r, ToAlias a, ToAliasReference a) => ToFrom (SubQuery (SqlQuery a)) a where
    toFrom (SubQuery q) = selectQuery q
instance (SqlSelect a r, ToAlias a, ToAliasReference a) => ToFrom (SqlQuery a) a where
    toFrom = selectQuery

selectQuery :: (SqlSelect a r, ToAlias a, ToAliasReference a) => SqlQuery a -> From a
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
