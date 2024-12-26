{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Esqueleto.Experimental.From.CommonTableExpression
    where

import qualified Control.Monad.Trans.Writer as W
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Experimental.From
import Database.Esqueleto.Experimental.From.SqlSetOperation
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)

-- | @WITH@ clause used to introduce a [Common Table Expression (CTE)](https://en.wikipedia.org/wiki/Hierarchical_and_recursive_queries_in_SQL#Common_table_expression).
-- CTEs are supported in most modern SQL engines and can be useful
-- in performance tuning. In Esqueleto, CTEs should be used as a
-- subquery memoization tactic. When writing plain SQL, CTEs
-- are sometimes used to organize the SQL code, in Esqueleto, this
-- is better achieved through function that return 'SqlQuery' values.
--
-- @
-- select $ do
-- cte <- with subQuery
-- cteResult <- from cte
-- where_ $ cteResult ...
-- pure cteResult
-- @
--
-- __WARNING__: In some SQL engines using a CTE can diminish performance.
-- In these engines the CTE is treated as an optimization fence. You should
-- always verify that using a CTE will in fact improve your performance
-- over a regular subquery.
--
-- Notably, in PostgreSQL prior to version 12, CTEs are always fully
-- calculated, which can potentially significantly pessimize queries. As of
-- PostgreSQL 12, non-recursive and side-effect-free queries may be inlined and
-- optimized accordingly if not declared @MATERIALIZED@ to get the previous
-- behaviour. See [the PostgreSQL CTE documentation](https://www.postgresql.org/docs/current/queries-with.html#id-1.5.6.12.7),
-- section Materialization, for more information. To use a @MATERIALIZED@ query
-- in Esquelto, see functions 'withMaterialized' and 'withRecursiveMaterialized'.
--
-- /Since: 3.4.0.0/
with :: ( ToAlias a
        , ToAliasReference a
        , SqlSelect a r
        ) => SqlQuery a -> SqlQuery (From a)
with query = do
    (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ query
    aliasedValue <- toAlias ret
    let aliasedQuery = Q $ W.WriterT $ pure (aliasedValue, sideData)
    ident <- newIdentFor (DBName "cte")
    let clause = CommonTableExpressionClause NormalCommonTableExpression (\_ _ -> "")  ident (\info -> toRawSql SELECT info aliasedQuery)
    Q $ W.tell mempty{sdCteClause = [clause]}
    ref <- toAliasReference ident aliasedValue
    pure $ From $ do
        newIdent <- newIdentFor (DBName "cte")
        localRef <- toAliasReference newIdent ref
        let makeLH info = useIdent info ident <> " AS " <> useIdent info newIdent
        pure (localRef, (\_ info -> (makeLH info, mempty)))

-- | @WITH@ @RECURSIVE@ allows one to make a recursive subquery, which can
-- reference itself. Like @WITH@, this is supported in most modern SQL engines.
-- Useful for hierarchical, self-referential data, like a tree of data.
--
-- @
-- select $ do
-- cte <- withRecursive
--          (do
--              person <- from $ table \@Person
--              where_ $ person ^. PersonId ==. val personId
--              pure person
--          )
--          unionAll_
--          (\\self -> do
--              (p :& f :& p2 :& pSelf) <- from self
--                       \`innerJoin\` $ table \@Follow
--                       \`on\` (\\(p :& f) ->
--                               p ^. PersonId ==. f ^. FollowFollower)
--                       \`innerJoin\` $ table \@Person
--                       \`on\` (\\(p :& f :& p2) ->
--                               f ^. FollowFollowed ==. p2 ^. PersonId)
--                       \`leftJoin\` self
--                       \`on\` (\\(_ :& _ :& p2 :& pSelf) ->
--                               just (p2 ^. PersonId) ==. pSelf ?. PersonId)
--              where_ $ isNothing (pSelf ?. PersonId)
--              groupBy (p2 ^. PersonId)
--              pure p2
--          )
-- from cte
-- @
--
-- /Since: 3.4.0.0/
withRecursive :: ( ToAlias a
                 , ToAliasReference a
                 , SqlSelect a r
                 )
              => SqlQuery a
              -> UnionKind
              -> (From a -> SqlQuery a)
              -> SqlQuery (From a)
withRecursive baseCase unionKind recursiveCase = do
    (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ baseCase
    aliasedValue <- toAlias ret
    let aliasedQuery = Q $ W.WriterT $ pure (aliasedValue, sideData)
    ident <- newIdentFor (DBName "cte")
    ref <- toAliasReference ident aliasedValue
    let refFrom = From (pure (ref, (\_ info -> (useIdent info ident, mempty))))
    let recursiveQuery = recursiveCase refFrom
    let noModifier _ _ = ""
    let clause = CommonTableExpressionClause RecursiveCommonTableExpression noModifier ident
                 (\info -> (toRawSql SELECT info aliasedQuery)
                        <> ("\n" <> (unUnionKind unionKind)  <> "\n", mempty)
                        <> (toRawSql SELECT info recursiveQuery)
                 )
    Q $ W.tell mempty{sdCteClause = [clause]}
    pure refFrom

newtype UnionKind = UnionKind { unUnionKind :: TLB.Builder }
instance Union_ UnionKind where
    union_ = UnionKind "UNION"
instance UnionAll_ UnionKind where
    unionAll_ = UnionKind "UNION ALL"
