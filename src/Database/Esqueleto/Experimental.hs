{-# LANGUAGE PatternSynonyms #-}

-- | This module contains a new way (introduced in 3.3.3.0) of using @FROM@ in
-- Haskell. The old method was a bit finicky and could permit runtime errors,
-- and this new way is both significantly safer and much more powerful.
--
-- This syntax will become the default syntax exported from the library in
-- version @3.6.0.0@. To use the old syntax, see "Database.Esqueleto.Legacy".
module Database.Esqueleto.Experimental
    ( -- * Setup
      -- $setup

      -- * Introduction
      -- $introduction

      -- * A New Syntax
      -- $new-syntax

      -- * Documentation

    -- ** Basic Queries
      from
    , table
    , Table(..)
    , SubQuery(..)
    , selectQuery

    -- ** Joins
    , (:&)(..)
    , on
    , innerJoin
    , innerJoinLateral
    , leftJoin
    , leftJoinLateral
    , rightJoin
    , fullOuterJoin
    , crossJoin
    , crossJoinLateral

      -- ** Set Operations
      -- $sql-set-operations
    , union_
    , Union(..)
    , unionAll_
    , UnionAll(..)
    , except_
    , Except(..)
    , intersect_
    , Intersect(..)
    , pattern SelectQuery

      -- ** Common Table Expressions
    , with
    , withRecursive

      -- ** Internals
    , From(..)
    , ToMaybe(..)
    , ToAlias(..)
    , ToAliasT
    , ToAliasReference(..)
    , ToAliasReferenceT
    , ToSqlSetOperation(..)

    -- * The Normal Stuff
    , where_
    , groupBy
    , orderBy
    , rand
    , asc
    , desc
    , limit
    , offset

    , distinct
    , distinctOn
    , don
    , distinctOnOrderBy
    , having
    , locking

    , sub_select
    , (^.)
    , (?.)

    , val
    , isNothing
    , just
    , nothing
    , joinV
    , withNonNull

    , countRows
    , count
    , countDistinct

    , not_
    , (==.)
    , (>=.)
    , (>.)
    , (<=.)
    , (<.)
    , (!=.)
    , (&&.)
    , (||.)

    , between
    , (+.)
    , (-.)
    , (/.)
    , (*.)

    , round_
    , ceiling_
    , floor_

    , min_
    , max_
    , sum_
    , avg_
    , castNum
    , castNumM

    , coalesce
    , coalesceDefault

    , lower_
    , upper_
    , trim_
    , ltrim_
    , rtrim_
    , length_
    , left_
    , right_

    , like
    , ilike
    , (%)
    , concat_
    , (++.)
    , castString

    , subList_select
    , valList
    , justList

    , in_
    , notIn
    , exists
    , notExists

    , set
    , (=.)
    , (+=.)
    , (-=.)
    , (*=.)
    , (/=.)

    , case_
    , toBaseId
    , subSelect
    , subSelectMaybe
    , subSelectCount
    , subSelectForeign
    , subSelectList
    , subSelectUnsafe
    , ToBaseId(..)
    , when_
    , then_
    , else_
    , Value(..)
    , ValueList(..)
    , OrderBy
    , DistinctOn
    , LockingKind(..)
    , LockableEntity(..)
    , SqlString

      -- ** Joins
    , InnerJoin(..)
    , CrossJoin(..)
    , LeftOuterJoin(..)
    , RightOuterJoin(..)
    , FullOuterJoin(..)
    , JoinKind(..)
    , OnClauseWithoutMatchingJoinException(..)
      -- *** Join Helpers
    , getTable
    , getTableMaybe
    , GetFirstTable(..)

      -- ** SQL backend
    , SqlQuery
    , SqlExpr
    , SqlEntity
    , select
    , selectOne
    , selectSource
    , delete
    , deleteCount
    , update
    , updateCount
    , insertSelect
    , insertSelectCount
    , (<#)
    , (<&>)

    -- ** Rendering Queries
    , renderQueryToText
    , renderQuerySelect
    , renderQueryUpdate
    , renderQueryDelete
    , renderQueryInsertInto

    -- ** Helpers
    , valkey
    , valJ
    , associateJoin

      -- ** Re-exports
      -- $reexports
    , deleteKey
    , module Database.Esqueleto.Internal.PersistentImport
    ) where

import Database.Esqueleto.Internal.Internal hiding (From, from, on)
import Database.Esqueleto.Internal.PersistentImport

import Database.Esqueleto.Experimental.From
import Database.Esqueleto.Experimental.From.CommonTableExpression
import Database.Esqueleto.Experimental.From.Join
import Database.Esqueleto.Experimental.From.SqlSetOperation
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Experimental.ToMaybe

-- $setup
--
-- If you're already using "Database.Esqueleto", then you can get
-- started using this module just by changing your imports slightly,
-- as well as enabling the [TypeApplications](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications) extension.
--
-- @
-- {-\# LANGUAGE TypeApplications \#-}
--
-- ...
--
-- import Database.Esqueleto.Experimental
-- @
--
-- Note: Prior to @esqueleto-3.3.4.0@, the @Database.Esqueleto.Experimental@
-- module did not reexport @Data.Esqueleto@.

----------------------------------------------------------------------

-- $introduction
--
-- This module is fully backwards-compatible extension to the @esqueleto@
-- EDSL that expands subquery functionality and enables
-- [SQL set operations](https://en.wikipedia.org/wiki/Set_operations_(SQL\))
-- to be written directly in Haskell. Specifically, this enables:
--
--   * Subqueries in 'JOIN' statements
--   * 'UNION'
--   * 'UNION' 'ALL'
--   * 'INTERSECT'
--   * 'EXCEPT'
--
-- As a consequence of this, several classes of runtime errors are now
-- caught at compile time. This includes missing 'on' clauses and improper
-- handling of @Maybe@ values in outer joins.
--
-- This module can be used in conjunction with the main "Database.Esqueleto"
-- module, but doing so requires qualified imports to avoid ambiguous
-- definitions of 'on' and 'from', which are defined in both modules.
--
-- Below we will give an overview of how to use this module and the
-- features it enables.

----------------------------------------------------------------------

-- $new-syntax
--
-- This module introduces a new syntax that serves to enable the aforementioned
-- features. This new syntax also changes how joins written in the @esqueleto@
-- EDSL to more closely resemble the underlying SQL.
--
-- For our examples, we'll use a schema similar to the one in the Getting Started
-- section of "Database.Esqueleto":
--
-- @
-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
--   Person
--     name String
--     age Int Maybe
--     deriving Eq Show
--   BlogPost
--     title String
--     authorId PersonId
--     deriving Eq Show
--   Follow
--     follower PersonId
--     followed PersonId
--     deriving Eq Show
-- |]
-- @
--
-- === Example 1: Simple select
--
-- Let's select all people who are named \"John\".
--
-- ==== "Database.Esqueleto":
--
-- @
-- select $
-- from $ \\people -> do
-- where_ (people ^. PersonName ==. val \"John\")
-- pure people
-- @
--
-- ==== "Database.Esqueleto.Experimental":
--
-- @
-- select $ do
-- people <- from $ table \@Person
-- where_ (people ^. PersonName ==. val \"John\")
-- pure people
-- @
--
--
-- === Example 2: Select with join
--
-- Let's select all people and their blog posts who are over
-- the age of 18.
--
-- ==== "Database.Esqueleto":
--
-- @
-- select $
-- from $ \\(people \`LeftOuterJoin\` blogPosts) -> do
-- on (people ^. PersonId ==. blogPosts ?. BlogPostAuthorId)
-- where_ (people ^. PersonAge >. val 18)
-- pure (people, blogPosts)
-- @
--
-- ==== "Database.Esqueleto.Experimental":
--
-- Here we use the ':&' operator to pattern match against the joined tables.
--
-- @
-- select $ do
-- (people :& blogPosts) <-
--     from $ table \@Person
--     \`leftJoin\` table \@BlogPost
--     \`on\` (\\(people :& blogPosts) ->
--             people ^. PersonId ==. blogPosts ?. BlogPostAuthorId)
-- where_ (people ^. PersonAge >. val 18)
-- pure (people, blogPosts)
-- @
--
-- === Example 3: Select with multi-table join
--
-- Let's select all people who follow a person named \"John\", including
-- the name of each follower.
--
-- ==== "Database.Esqueleto":
--
-- @
-- select $
-- from $ \\(
--  people1
--  \`InnerJoin\` followers
--  \`InnerJoin\` people2
-- ) -> do
-- on (people1 ^. PersonId ==. followers ^. FollowFollowed)
-- on (followers ^. FollowFollower ==. people2 ^. PersonId)
-- where_ (people1 ^. PersonName ==. val \"John\")
-- pure (followers, people2)
-- @
--
-- ==== "Database.Esqueleto.Experimental":
--
-- In this version, with each successive 'on' clause, only the tables
-- we have already joined into are in scope, so we must pattern match
-- accordingly. In this case, in the second 'innerJoin', we do not use
-- the first `Person` reference, so we use @_@ as a placeholder to
-- ignore it. This prevents a possible runtime error where a table
-- is referenced before it appears in the sequence of 'JOIN's.
--
-- @
-- select $ do
-- (people1 :& followers :& people2) <-
--     from $ table \@Person
--     \`innerJoin` table \@Follow
--     \`on\` (\\(people1 :& followers) ->
--             people1 ^. PersonId ==. followers ^. FollowFollowed)
--     \`innerJoin` table \@Person
--     \`on\` (\\(_ :& followers :& people2) ->
--             followers ^. FollowFollower ==. people2 ^. PersonId)
-- where_ (people1 ^. PersonName ==. val \"John\")
-- pure (followers, people2)
-- @
--
-- === Example 4: Counting results of a subquery
--
-- Let's count the number of people who have posted at least 10 posts
--
-- ==== "Database.Esqueleto":
--
-- @
-- select $ pure $ subSelectCount $
-- from $ \\(
--   people
--   \`InnerJoin\` blogPosts
-- ) -> do
-- on (people ^. PersonId ==. blogPosts ^. BlogPostAuthorId)
-- groupBy (people ^. PersonId)
-- having ((count $ blogPosts ^. BlogPostId) >. val 10)
-- pure people
-- @
--
-- ==== "Database.Esqueleto.Experimental":
--
-- @
-- select $ do
-- peopleWithPosts <-
--   from $ do
--     (people :& blogPosts) <-
--       from $ table \@Person
--       \`innerJoin\` table \@BlogPost
--       \`on\` (\\(p :& bP) ->
--               p ^. PersonId ==. bP ^. BlogPostAuthorId)
--     groupBy (people ^. PersonId)
--     having ((count $ blogPosts ^. BlogPostId) >. val 10)
--     pure people
-- pure $ count (peopleWithPosts ^. PersonId)
-- @
--
-- We now have the ability to refactor this
--
-- === Example 5: Sorting the results of a UNION with limits
--
-- Out of all of the posts created by a person and the people they follow,
-- generate a list of the first 25 posts, sorted alphabetically.
--
-- ==== "Database.Esqueleto":
--
-- Since 'UNION' is not supported, this requires using `Database.Esqueleto.rawSql`. (Not shown)
--
-- ==== "Database.Esqueleto.Experimental":
--
-- Since this module supports all set operations (see `SqlSetOperation`), we can use
-- `Union` to write this query.
--
-- @
-- select $ do
-- (authors, blogPosts) <- from $
--   (do
--     (author :& blogPost) <-
--       from $ table \@Person
--       \`innerJoin\` table \@BlogPost
--       \`on\` (\\(a :& bP) ->
--               a ^. PersonId ==. bP ^. BlogPostAuthorId)
--     where_ (author ^. PersonId ==. val currentPersonId)
--     pure (author, blogPost)
--   )
--   \`union_\`
--   (do
--     (follow :& blogPost :& author) <-
--       from $ table \@Follow
--       \`innerJoin\` table \@BlogPost
--       \`on\` (\\(f :& bP) ->
--               f ^. FollowFollowed ==. bP ^. BlogPostAuthorId)
--       \`innerJoin\` table \@Person
--       \`on\` (\\(_ :& bP :& a) ->
--               bP ^. BlogPostAuthorId ==. a ^. PersonId)
--     where_ (follow ^. FollowFollower ==. val currentPersonId)
--     pure (author, blogPost)
--   )
-- orderBy [ asc (blogPosts ^. BlogPostTitle) ]
-- limit 25
-- pure (authors, blogPosts)
-- @
--
-- === Example 6: LATERAL JOIN
--
-- As of version @3.4.0.0@, lateral subquery joins are supported.
--
--
-- @
-- select $ do
-- (salesPerson :& maxSaleAmount :& maxSaleCustomerName) <-
--   from $ table \@SalesPerson
--   \`crossJoinLateral\` (\\salesPerson -> do
--         sales <- from $ table \@Sale
--         where_ $ sales ^. SaleSalesPersonId ==. salesPerson ^. SalesPersonId
--         pure $ max_ (sales ^. SaleAmount)
--         )
--   \`crossJoinLateral\` (\\(salesPerson :& maxSaleAmount) -> do
--         sales <- from $ table \@Sale
--         where_ $ sales ^. SaleSalesPersonId ==. salesPerson ^. SalesPersonId
--              &&. sales ^. SaleAmount ==. maxSaleAmount
--         pure $ sales ^. SaleCustomerName)
--         )
-- pure (salesPerson ^. SalesPersonName, maxSaleAmount, maxSaleCustomerName)
-- @
--
-- This is the equivalent to the following SQL (example taken from the
-- [MySQL Lateral Derived Table](https://dev.mysql.com/doc/refman/8.0/en/lateral-derived-tables.html)
-- documentation):
--
-- @
-- SELECT
--   salesperson.name,
--   max_sale.amount,
--   max_sale_customer.customer_name
-- FROM
--   salesperson,
--   -- calculate maximum size, cache it in transient derived table max_sale
--   LATERAL
--   (SELECT MAX(amount) AS amount
--     FROM all_sales
--     WHERE all_sales.salesperson_id = salesperson.id)
--   AS max_sale,
--   LATERAL
--   (SELECT customer_name
--     FROM all_sales
--     WHERE all_sales.salesperson_id = salesperson.id
--     AND all_sales.amount =
--         -- the cached maximum size
--         max_sale.amount)
--   AS max_sale_customer;
-- @

-- $sql-set-operations
--
-- Data type that represents SQL set operations. This includes
-- 'UNION', 'UNION' 'ALL', 'EXCEPT', and 'INTERSECT'. These types form
-- a binary tree, with @SqlQuery@ values on the leaves.
--
-- Each function corresponding to the aforementioned set operations
-- can be used as an infix in a @from@ to help with readability
-- and lead to code that closely resembles the underlying SQL. For example,
--
-- @
-- select $ from $
--   (do
--      a <- from $ table @A
--      pure $ a ^. ASomeCol
--   )
--   \`union_\`
--   (do
--      b <- from $ table @B
--      pure $ b ^. BSomeCol
--   )
-- @
--
-- is translated into
--
-- @
-- SELECT * FROM (
--   (SELECT a.some_col FROM a)
--   UNION
--   (SELECT b.some_col FROM b)
-- )
-- @
--
