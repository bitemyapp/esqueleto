{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.SubQuery
    where

import qualified Control.Monad.Trans.Writer as W
import Database.Esqueleto.Experimental.Internal
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)
import Database.Esqueleto.Internal.PersistentImport (DBName(..))

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
