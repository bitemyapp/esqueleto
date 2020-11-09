{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.From
    where

import qualified Control.Monad.Trans.Writer as W
import Data.Proxy
import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)
import Database.Esqueleto.Internal.PersistentImport
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference

-- | 'FROM' clause, used to bring entities into scope.
--
-- Internally, this function uses the `From` datatype and the
-- `From` typeclass. Unlike the old `Database.Esqueleto.from`,
-- this does not take a function as a parameter, but rather
-- a value that represents a 'JOIN' tree constructed out of
-- instances of `From`. This implementation eliminates certain
-- types of runtime errors by preventing the construction of
-- invalid SQL (e.g. illegal nested-@from@).
from :: From a  => a -> SqlQuery (FromT a)
from parts = do
    (a, clause) <- runFrom parts
    Q $ W.tell mempty{sdFromClause=[clause]}
    pure a

class From a where
    type FromT a
    runFrom :: a -> SqlQuery (FromT a, FromClause)

-- | Data type for bringing a Table into scope in a JOIN tree
--
-- @
-- select $ from $ Table \@People
-- @
data Table a = Table

instance PersistEntity a => From (Table a) where
    type FromT (Table a) = SqlExpr (Entity a)
    runFrom e@Table = do
        let ed = entityDef $ getVal e
        ident <- newIdentFor (entityDB ed)
        let entity = EEntity ident
        pure $ (entity, FromStart ident ed)
          where
            getVal ::  Table ent -> Proxy ent
            getVal = const Proxy


{-# DEPRECATED SubQuery "/Since: 3.4.0.0/ - It is no longer necessary to tag 'SqlQuery' values with @SubQuery@" #-}
newtype SubQuery a = SubQuery a

instance
    ( ToAlias a
    , ToAliasReference a
    , SqlSelect a r
    )
  =>
    From (SqlQuery a)
  where
    type FromT (SqlQuery a) = a
    runFrom subquery =
        fromSubQuery NormalSubQuery subquery

instance
    ( ToAlias a
    , ToAliasReference a
    , SqlSelect a r
    )
  =>
    From (SubQuery (SqlQuery a))
  where
    type FromT (SubQuery (SqlQuery a)) = a
    runFrom (SubQuery subquery) =
        fromSubQuery NormalSubQuery subquery

fromSubQuery
    ::
    ( SqlSelect a r
    , ToAlias a
    , ToAliasReference a
    )
    => SubQueryType -> SqlQuery a -> SqlQuery (a, FromClause)
fromSubQuery subqueryType subquery = do
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
    pure (ref , FromQuery subqueryAlias (\info -> toRawSql SELECT info aliasedQuery) subqueryType)
