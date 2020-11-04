{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Esqueleto.Experimental.CommonTableExpression
    where

import qualified Control.Monad.Trans.Writer as W
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Experimental.Internal
import Database.Esqueleto.Experimental.SqlSetOperation
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)
import Database.Esqueleto.Internal.PersistentImport (DBName(..))

data CommonTableExpression ref = CommonTableExpression Ident ref
instance From (CommonTableExpression ref) where
    type FromT (CommonTableExpression ref) = ref
    runFrom (CommonTableExpression ident ref) =
        pure (ref, FromIdent ident)

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
-- /Since: 3.4.0.0/
with :: ( ToAlias a
        , ToAliasReference a
        , SqlSelect a r
        ) => SqlQuery a -> SqlQuery (CommonTableExpression a)
with query = do
    (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ query
    aliasedValue <- toAlias ret
    let aliasedQuery = Q $ W.WriterT $ pure (aliasedValue, sideData)
    ident <- newIdentFor (DBName "cte")
    let clause = CommonTableExpressionClause NormalCommonTableExpression ident (\info -> toRawSql SELECT info aliasedQuery)
    Q $ W.tell mempty{sdCteClause = [clause]}
    ref <- toAliasReference ident aliasedValue
    pure $ CommonTableExpression ident ref

-- | @WITH@ @RECURSIVE@ allows one to make a recursive subquery, which can
-- reference itself. Like @WITH@, this is supported in most modern SQL engines.
-- Useful for hierarchical, self-referential data, like a tree of data.
--
-- @
-- select $ do
-- cte <- withRecursive
--          (do $
--              person <- from $ Table \@Person
--              where_ $ person ^. PersonId ==. val personId
--              pure person
--          )
--          unionAll_
--          (\\self -> do $
--              (p :& f :& p2 :& pSelf) <- from self
--                       \`InnerJoin\` $ Table \@Follow
--                       \`on\` (\\(p :& f) ->
--                               p ^. PersonId ==. f ^. FollowFollower)
--                       \`InnerJoin\` $ Table \@Person
--                       \`on\` (\\(p :& f :& p2) ->
--                               f ^. FollowFollowed ==. p2 ^. PersonId)
--                       \`LeftOuterJoin\` self
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
                 , RecursiveCteUnion unionKind
                 )
              => SqlQuery a
              -> unionKind
              -> (CommonTableExpression a -> SqlQuery a)
              -> SqlQuery (CommonTableExpression a)
withRecursive baseCase unionKind recursiveCase = do
    (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ baseCase
    aliasedValue <- toAlias ret
    let aliasedQuery = Q $ W.WriterT $ pure (aliasedValue, sideData)
    ident <- newIdentFor (DBName "cte")
    ref <- toAliasReference ident aliasedValue
    let refFrom = CommonTableExpression ident ref
    let recursiveQuery = recursiveCase refFrom
    let clause = CommonTableExpressionClause RecursiveCommonTableExpression ident
                 (\info -> (toRawSql SELECT info aliasedQuery)
                        <> (unionKeyword unionKind, mempty)
                        <> (toRawSql SELECT info recursiveQuery)
                 )
    Q $ W.tell mempty{sdCteClause = [clause]}
    pure refFrom

class RecursiveCteUnion a where
    unionKeyword :: a -> TLB.Builder

instance RecursiveCteUnion (a -> b -> Union a b) where
    unionKeyword _ = "\nUNION\n"

instance RecursiveCteUnion (a -> b -> UnionAll a b) where
    unionKeyword _ = "\nUNION ALL\n"
