{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs #-}
-- | The @esqueleto@ EDSL (embedded domain specific language).
-- This module replaces @Database.Persist@, so instead of
-- importing that module you should just import this one:
--
-- @
-- -- For a module using just esqueleto.
-- import Database.Esqueleto
-- @
--
-- If you need to use @persistent@'s default support for queries
-- as well, either import it qualified:
--
-- @
-- -- For a module that mostly uses esqueleto.
-- import Database.Esqueleto
-- import qualified Database.Persistent as P
-- @
--
-- or import @esqueleto@ itself qualified:
--
-- @
-- -- For a module that uses esqueleto just on some queries.
-- import Database.Persistent
-- import qualified Database.Esqueleto as E
-- @
--
-- Other than identifier name clashes, @esqueleto@ does not
-- conflict with @persistent@ in any way.
module Database.Esqueleto
  ( -- * Setup
    -- $setup

    -- * Introduction
    -- $introduction

    -- * Getting started
    -- $gettingstarted

    -- * @esqueleto@'s Language
    Esqueleto( where_, on, groupBy, orderBy, rand, asc, desc, limit, offset
             , distinct, distinctOn, don, distinctOnOrderBy, having, locking
             , sub_select, sub_selectDistinct, (^.), (?.)
             , val, isNothing, just, nothing, joinV
             , countRows, count, countDistinct
             , not_, (==.), (>=.), (>.), (<=.), (<.), (!=.), (&&.), (||.)
             , (+.), (-.), (/.), (*.)
             , random_, round_, ceiling_, floor_
             , min_, max_, sum_, avg_, castNum, castNumM
             , coalesce, coalesceDefault
             , lower_, like, ilike, (%), concat_, (++.), castString
             , subList_select, subList_selectDistinct, valList, justList
             , in_, notIn, exists, notExists
             , set, (=.), (+=.), (-=.), (*=.), (/=.)
             , case_, toBaseId)
  , ToBaseId(..)
  , when_
  , then_
  , else_
  , from
  , Value(..)
  , unValue
  , ValueList(..)
  , OrderBy
  , DistinctOn
  , LockingKind(..)
  , SqlString
    -- ** Joins
  , InnerJoin(..)
  , CrossJoin(..)
  , LeftOuterJoin(..)
  , RightOuterJoin(..)
  , FullOuterJoin(..)
  , OnClauseWithoutMatchingJoinException(..)

    -- * SQL backend
  , SqlQuery
  , SqlExpr
  , SqlEntity
  , select
  , selectDistinct
  , selectSource
  , selectDistinctSource
  , delete
  , deleteCount
  , update
  , updateCount
  , insertSelect
  , insertSelectCount
  , insertSelectDistinct
  , (<#)
  , (<&>)

    -- * RDBMS-specific modules
    -- $rdbmsSpecificModules

    -- * Helpers
  , valkey
  , valJ

    -- * Re-exports
    -- $reexports
  , deleteKey
  , module Database.Esqueleto.Internal.PersistentImport
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Int (Int64)
import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
import Database.Esqueleto.Internal.PersistentImport
import qualified Database.Persist


-- $setup
--
-- If you're already using @persistent@, then you're ready to use
-- @esqueleto@, no further setup is needed.  If you're just
-- starting a new project and would like to use @esqueleto@, take
-- a look at @persistent@'s book first
-- (<http://www.yesodweb.com/book/persistent>) to learn how to
-- define your schema.


----------------------------------------------------------------------


-- $introduction
--
-- The main goals of @esqueleto@ are to:
--
--   * Be easily translatable to SQL.  When you take a look at a
--   @esqueleto@ query, you should be able to know exactly how
--   the SQL query will end up.  (As opposed to being a
--   relational algebra EDSL such as HaskellDB, which is
--   non-trivial to translate into SQL.)
--
--   * Support the most widely used SQL features.  We'd like you to be
--   able to use @esqueleto@ for all of your queries, no
--   exceptions.  Send a pull request or open an issue on our
--   project page (<https://github.com/prowdsponsor/esqueleto>) if
--   there's anything missing that you'd like to see.
--
--   * Be as type-safe as possible.  We strive to provide as many
--   type checks as possible.  If you get bitten by some invalid
--   code that type-checks, please open an issue on our project
--   page so we can take a look.
--
-- However, it is /not/ a goal to be able to write portable SQL.
-- We do not try to hide the differences between DBMSs from you,
-- and @esqueleto@ code that works for one database may not work
-- on another.  This is a compromise we have to make in order to
-- give you as much control over the raw SQL as possible without
-- losing too much convenience.  This also means that you may
-- type-check a query that doesn't work on your DBMS.


----------------------------------------------------------------------


-- $gettingstarted
--
-- We like clean, easy-to-read EDSLs.  However, in order to
-- achieve this goal we've used a lot of type hackery, leading to
-- some hard-to-read type signatures.  On this section, we'll try
-- to build some intuition about the syntax.
--
-- For the following examples, we'll use this example schema:
--
-- @
-- share [mkPersist sqlSettings, mkMigrate \"migrateAll\"] [persist|
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
-- Most of @esqueleto@ was created with @SELECT@ statements in
-- mind, not only because they're the most common but also
-- because they're the most complex kind of statement.  The most
-- simple kind of @SELECT@ would be:
--
-- @
-- SELECT *
-- FROM Person
-- @
--
-- In @esqueleto@, we may write the same query above as:
--
-- @
-- do people <- 'select' $
--              'from' $ \\person -> do
--              return person
--    liftIO $ mapM_ (putStrLn . personName . entityVal) people
-- @
--
-- The expression above has type @SqlPersist m ()@, while
-- @people@ has type @[Entity Person]@.  The query above will be
-- translated into exactly the same query we wrote manually, but
-- instead of @SELECT *@ it will list all entity fields (using
-- @*@ is not robust).  Note that @esqueleto@ knows that we want
-- an @Entity Person@ just because of the @personName@ that we're
-- printing later.
--
-- However, most of the time we need to filter our queries using
-- @WHERE@.  For example:
--
-- @
-- SELECT *
-- FROM Person
-- WHERE Person.name = \"John\"
-- @
--
-- In @esqueleto@, we may write the same query above as:
--
-- @
-- 'select' $
-- 'from' $ \\p -> do
-- 'where_' (p '^.' PersonName '==.' 'val' \"John\")
-- return p
-- @
--
-- Although @esqueleto@'s code is a bit more noisy, it's has
-- almost the same structure (save from the @return@).  The
-- @('^.')@ operator is used to project a field from an entity.
-- The field name is the same one generated by @persistent@'s
-- Template Haskell functions.  We use 'val' to lift a constant
-- Haskell value into the SQL query.
--
-- Another example would be:
--
-- @
-- SELECT *
-- FROM Person
-- WHERE Person.age >= 18
-- @
--
-- In @esqueleto@, we may write the same query above as:
--
-- @
-- 'select' $
-- 'from' $ \\p -> do
-- 'where_' (p '^.' PersonAge '>=.' 'just' ('val' 18))
-- return p
-- @
--
-- Since @age@ is an optional @Person@ field, we use 'just' to lift
-- @'val' 18 :: SqlExpr (Value Int)@ into @just ('val' 18) ::
-- SqlExpr (Value (Maybe Int))@.
--
-- Implicit joins are represented by tuples.  For example, to get
-- the list of all blog posts and their authors, we could write:
--
-- @
-- SELECT BlogPost.*, Person.*
-- FROM BlogPost, Person
-- WHERE BlogPost.authorId = Person.id
-- ORDER BY BlogPost.title ASC
-- @
--
-- In @esqueleto@, we may write the same query above as:
--
-- @
-- 'select' $
-- 'from' $ \\(b, p) -> do
-- 'where_' (b '^.' BlogPostAuthorId '==.' p '^.' PersonId)
-- 'orderBy' ['asc' (b '^.' BlogPostTitle)]
-- return (b, p)
-- @
--
-- However, you may want your results to include people who don't
-- have any blog posts as well using a @LEFT OUTER JOIN@:
--
-- @
-- SELECT Person.*, BlogPost.*
-- FROM Person LEFT OUTER JOIN BlogPost
-- ON Person.id = BlogPost.authorId
-- ORDER BY Person.name ASC, BlogPost.title ASC
-- @
--
-- In @esqueleto@, we may write the same query above as:
--
-- @
-- 'select' $
-- 'from' $ \\(p `'LeftOuterJoin`` mb) -> do
-- 'on' ('just' (p '^.' PersonId) '==.' mb '?.' BlogPostAuthorId)
-- 'orderBy' ['asc' (p '^.' PersonName), 'asc' (mb '?.' BlogPostTitle)]
-- return (p, mb)
-- @
--
-- On a @LEFT OUTER JOIN@ the entity on the right hand side may
-- not exist (i.e. there may be a @Person@ without any
-- @BlogPost@s), so while @p :: SqlExpr (Entity Person)@, we have
-- @mb :: SqlExpr (Maybe (Entity BlogPost))@.  The whole
-- expression above has type @SqlPersist m [(Entity Person, Maybe
-- (Entity BlogPost))]@.  Instead of using @(^.)@, we used
-- @('?.')@ to project a field from a @Maybe (Entity a)@.
--
-- We are by no means limited to joins of two tables, nor by
-- joins of different tables.  For example, we may want a list
-- of the @Follow@ entity:
--
-- @
-- SELECT P1.*, Follow.*, P2.*
-- FROM Person AS P1
-- INNER JOIN Follow ON P1.id = Follow.follower
-- INNER JOIN P2 ON P2.id = Follow.followed
-- @
--
-- In @esqueleto@, we may write the same query above as:
--
-- @
-- 'select' $
-- 'from' $ \\(p1 `'InnerJoin`` f `'InnerJoin`` p2) -> do
-- 'on' (p2 '^.' PersonId '==.' f '^.' FollowFollowed)
-- 'on' (p1 '^.' PersonId '==.' f '^.' FollowFollower)
-- return (p1, f, p2)
-- @
--
-- /Note carefully that the order of the ON clauses is/
-- /reversed!/ You're required to write your 'on's in reverse
-- order because that helps composability (see the documentation
-- of 'on' for more details).
--
-- We also currently support @UPDATE@ and @DELETE@ statements.
-- For example:
--
-- @
-- do 'update' $ \\p -> do
--      'set' p [ PersonName '=.' 'val' \"João\" ]
--      'where_' (p '^.' PersonName '==.' 'val' \"Joao\")
--    'delete' $
--      'from' $ \\p -> do
--      'where_' (p '^.' PersonAge '<.' 'just' ('val' 14))
-- @
--
-- The results of queries can also be used for insertions.
-- In @SQL@, we might write the following, inserting a new blog
-- post for every user:
--
-- @
-- INSERT INTO BlogPost
-- SELECT ('Group Blog Post', id)
-- FROM Person
-- @
--
-- In @esqueleto@, we may write the same query above as:
--
-- @
--  'insertSelect' $ 'from' $ \\p->
--  return $ BlogPost '<#' \"Group Blog Post\" '<&>' (p '^.' PersonId)
-- @
--
-- Individual insertions can be performed through Persistent's
-- 'insert' function, reexported for convenience.


----------------------------------------------------------------------


-- $reexports
--
-- We re-export many symbols from @persistent@ for convenince:
--
--  * \"Store functions\" from "Database.Persist".
--
--  * Everything from "Database.Persist.Class" except for
--    @PersistQuery@ and @delete@ (use 'deleteKey' instead).
--
--  * Everything from "Database.Persist.Types" except for
--    @Update@, @SelectOpt@, @BackendSpecificFilter@ and @Filter@.
--
--  * Everything from "Database.Persist.Sql" except for
--    @deleteWhereCount@ and @updateWhereCount@.


----------------------------------------------------------------------


-- $rdbmsSpecificModules
--
-- There are many differences between SQL syntax and functions
-- supported by different RDBMSs.  Since version 2.2.8,
-- @esqueleto@ includes modules containing functions that are
-- specific to a given RDBMS.
--
--  * PostgreSQL: "Database.Esqueleto.PostgreSQL".
--
-- In order to use these functions, you need to explicitly import
-- their corresponding modules, they're not re-exported here.


----------------------------------------------------------------------


-- | @valkey i = 'val' . 'toSqlKey'@
-- (<https://github.com/prowdsponsor/esqueleto/issues/9>).
valkey :: (Esqueleto query expr backend, ToBackendKey SqlBackend entity, PersistField (Key entity)) =>
          Int64 -> expr (Value (Key entity))
valkey = val . toSqlKey


-- | @valJ@ is like @val@ but for something that is already a @Value@. The use
-- case it was written for was, given a @Value@ lift the @Key@ for that @Value@
-- into the query expression in a type safe way. However, the implementation is
-- more generic than that so we call it @valJ@.
--
-- Its important to note that the input entity and the output entity are
-- constrained to be the same by the type signature on the function
-- (<https://github.com/prowdsponsor/esqueleto/pull/69>).
--
-- /Since: 1.4.2/
valJ :: (Esqueleto query expr backend, PersistField (Key entity)) =>
        Value (Key entity) -> expr (Value (Key entity))
valJ = val . unValue


----------------------------------------------------------------------


-- | Synonym for 'Database.Persist.Store.delete' that does not
-- clash with @esqueleto@'s 'delete'.
deleteKey :: ( PersistStore (PersistEntityBackend val)
             , MonadIO m
             , PersistEntity val )
          => Key val -> ReaderT (PersistEntityBackend val) m ()
deleteKey = Database.Persist.delete
