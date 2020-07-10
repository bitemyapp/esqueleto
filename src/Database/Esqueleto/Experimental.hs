{-# LANGUAGE CPP
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , MultiParamTypeClasses
           , TypeOperators
           , TypeFamilies
           , UndecidableInstances
           , OverloadedStrings
 #-}

module Database.Esqueleto.Experimental
    ( -- * Setup
      -- $setup

      -- * Introduction
      -- $introduction

      -- * A New Syntax
      -- $new-syntax

      -- * Documentation

      SqlSetOperation(..)
    , From(..)
    , on
    , from
    , (:&)(..)
      -- * Internals
    , ToFrom(..)
    , ToFromT
    , ToMaybe(..)
    , ToMaybeT
    , ToAlias(..)
    , ToAliasT
    , ToAliasReference(..)
    , ToAliasReferenceT
    )
    where

import qualified Control.Monad.Trans.Writer as W
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class (lift)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif
import Data.Proxy (Proxy(..))
import Database.Esqueleto.Internal.PersistentImport
import Database.Esqueleto.Internal.Internal
          ( SqlExpr(..)
          , InnerJoin(..)
          , CrossJoin(..)
          , LeftOuterJoin(..)
          , RightOuterJoin(..)
          , FullOuterJoin(..)
          , FromClause(..)
          , SqlQuery(..)
          , SideData(..)
          , Value(..)
          , JoinKind(..)
          , newIdentFor
          , SqlSelect(..)
          , Mode(..)
          , toRawSql
          , Ident(..)
          , to3, to4, to5, to6, to7, to8
          , from3, from4, from5, from6, from7, from8
          , veryUnsafeCoerceSqlExprValue
          , parens
          )
import GHC.TypeLits

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
-- import Database.Esqueleto hiding (on, from)
-- import Database.Esqueleto.Experimental
-- @

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
-- people <- from $ Table \@Person
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
--     from $ Table \@Person
--     \`LeftOuterJoin\` Table \@BlogPost
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
-- accordingly. In this case, in the second 'InnerJoin', we do not use
-- the first `Person` reference, so we use @_@ as a placeholder to
-- ignore it. This prevents a possible runtime error where a table
-- is referenced before it appears in the sequence of 'JOIN's.
--
-- @
-- select $ do
-- (people1 :& followers :& people2) <-
--     from $ Table \@Person
--     \`InnerJoin` Table \@Follow
--     \`on\` (\\(people1 :& followers) ->
--             people1 ^. PersonId ==. followers ^. FollowFollowed)
--     \`InnerJoin` Table \@Person
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
--   from $ SelectQuery $ do
--     (people :& blogPosts) <-
--       from $ Table \@Person
--       \`InnerJoin\` Table \@BlogPost
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
--   (SelectQuery $ do
--     (author :& blogPost) <-
--       from $ Table \@Person
--       \`InnerJoin\` Table \@BlogPost
--       \`on\` (\\(a :& bP) ->
--               a ^. PersonId ==. bP ^. BlogPostAuthorId)
--     where_ (author ^. PersonId ==. val currentPersonId)
--     pure (author, blogPost)
--   )
--   \`Union\`
--   (SelectQuery $ do
--     (follow :& blogPost :& author) <-
--       from $ Table \@Follow
--       \`InnerJoin\` Table \@BlogPost
--       \`on\` (\\(f :& bP) ->
--               f ^. FollowFollowed ==. bP ^. BlogPostAuthorId)
--       \`InnerJoin\` Table \@Person
--       \`on\` (\\(_ :& bP :& a) ->
--               bP ^. BlogPostAuthorId ==. a ^. PersonId)
--     where_ (follow ^. FollowFollower ==. val currentPersonId)
--     pure (author, blogPost)
--   )
-- orderBy [ asc (blogPosts ^. BlogPostTitle) ]
-- limit 25
-- pure (authors, blogPosts)
-- @

-- | A left-precedence pair. Pronounced \"and\". Used to represent expressions
-- that have been joined together.
--
-- The precedence behavior can be demonstrated by:
--
-- @
-- a :& b :& c == ((a :& b) :& c)
-- @
--
-- See the examples at the beginning of this module to see how this
-- operator is used in 'JOIN' operations.
data (:&) a b = a :& b
infixl 2 :&

-- | Data type that represents SQL set operations. This includes
-- 'UNION', 'UNION' 'ALL', 'EXCEPT', and 'INTERSECT'. This data
-- type is defined as a binary tree, with @SelectQuery@ on the leaves.
--
-- Each constructor corresponding to the aforementioned set operations
-- can be used as an infix function in a @from@ to help with readability
-- and lead to code that closely resembles the underlying SQL. For example,
--
-- @
-- select $ from $
--   (SelectQuery ...)
--   \`Union\`
--   (SelectQuery ...)
-- @
--
-- is translated into
--
-- @
-- SELECT * FROM (
--   (SELECT * FROM ...)
--   UNION
--   (SELECT * FROM ...)
-- )
-- @
--
-- @SelectQuery@ can be used without any of the set operations to construct
-- a subquery. This can be used in 'JOIN' trees. For example,
--
-- @
-- select $ from $
--   Table \@SomeTable
--   \`InnerJoin\` (SelectQuery ...)
--   \`on\` ...
-- @
--
-- is translated into
--
-- @
-- SELECT *
-- FROM SomeTable
-- INNER JOIN (SELECT * FROM ...)
-- ON ...
-- @
data SqlSetOperation a =
    Union (SqlSetOperation a) (SqlSetOperation a)
  | UnionAll (SqlSetOperation a) (SqlSetOperation a)
  | Except (SqlSetOperation a) (SqlSetOperation a)
  | Intersect (SqlSetOperation a) (SqlSetOperation a)
  | SelectQuery  (SqlQuery a)
  | SelectQueryP (SqlQuery a)

-- | Data type that represents the syntax of a 'JOIN' tree. In practice,
-- only the @Table@ constructor is used directly when writing queries. For example,
--
-- @
-- select $ from $ Table \@People
-- @
data From a where
  Table         :: PersistEntity ent => From (SqlExpr (Entity ent))
  SubQuery      :: (SqlSelect a' r, SqlSelect a'' r', ToAlias a, a' ~ ToAliasT a, ToAliasReference a', ToAliasReferenceT a' ~ a'')
                => SqlQuery a
                -> From a''
  SqlSetOperation :: (SqlSelect a' r, ToAlias a, a' ~ ToAliasT a, ToAliasReference a', ToAliasReferenceT a' ~ a'')
                  => SqlSetOperation a
                  -> From a''
  InnerJoinFrom :: From a
                -> (From b, (a :& b) -> SqlExpr (Value Bool))
                -> From (a :& b)
  CrossJoinFrom :: From a
                -> From b
                -> From (a :& b)
  LeftJoinFrom  :: ToMaybe b
                => From a
                -> (From b, (a :& ToMaybeT b) -> SqlExpr (Value Bool))
                -> From (a :& ToMaybeT b)
  RightJoinFrom :: ToMaybe a
                => From a
                -> (From b, (ToMaybeT a :& b) -> SqlExpr (Value Bool))
                -> From (ToMaybeT a :& b)
  FullJoinFrom  :: (ToMaybe a, ToMaybe b )
                => From a
                -> (From b, (ToMaybeT a :& ToMaybeT b) -> SqlExpr (Value Bool))
                -> From (ToMaybeT a :& ToMaybeT b)

-- | An @ON@ clause that describes how two tables are related. This should be
-- used as an infix operator after a 'JOIN'. For example,
--
-- @
-- select $
-- from $ Table \@Person
-- \`InnerJoin\` Table \@BlogPost
-- \`on\` (\\(p :& bP) ->
--         p ^. PersonId ==. bP ^. BlogPostAuthorId)
-- @
--
on :: ToFrom a => a -> (b -> SqlExpr (Value Bool)) -> (a, b -> SqlExpr (Value Bool))
on = (,)
infix 9 `on`

type JoinErrorMsg jk = 'Text "Missing on statement for " ':<>: 'Text jk

type family ToFromT a where
  ToFromT (From a) = a
  ToFromT (SqlSetOperation a) = ToAliasReferenceT (ToAliasT a)
  ToFromT (LeftOuterJoin a (b, c -> SqlExpr (Value Bool))) = c
  ToFromT (FullOuterJoin a (b, c -> SqlExpr (Value Bool))) = c
  ToFromT (RightOuterJoin a (b, c -> SqlExpr (Value Bool))) = c
  ToFromT (InnerJoin a (b, c -> SqlExpr (Value Bool))) = c
  ToFromT (CrossJoin a b) = (ToFromT a :& ToFromT b)
  ToFromT (InnerJoin a b) = TypeError (JoinErrorMsg "InnerJoin")
  ToFromT (LeftOuterJoin a b) = TypeError (JoinErrorMsg "LeftOuterJoin")
  ToFromT (RightOuterJoin a b) = TypeError (JoinErrorMsg "RightOuterJoin")
  ToFromT (FullOuterJoin a b) = TypeError (JoinErrorMsg "FullOuterJoin")

{-- Type class magic to allow the use of the `InnerJoin` family of data constructors in from --}
class ToFrom a where
  toFrom :: a -> From (ToFromT a)

instance ToFrom (From a) where
  toFrom = id

instance {-# OVERLAPPABLE #-} ToFrom (InnerJoin a b) where
  toFrom = undefined
instance {-# OVERLAPPABLE #-} ToFrom (LeftOuterJoin a b) where
  toFrom = undefined
instance {-# OVERLAPPABLE #-} ToFrom (RightOuterJoin a b) where
  toFrom = undefined
instance {-# OVERLAPPABLE #-} ToFrom (FullOuterJoin a b) where
  toFrom = undefined

instance (SqlSelect a' r,SqlSelect a'' r', ToAlias a, a' ~ ToAliasT a, ToAliasReference a', ToAliasReferenceT a' ~ a'')  => ToFrom (SqlSetOperation a) where
  -- If someone uses just a plain SelectQuery it should behave like a normal subquery
  toFrom (SelectQuery q) = SubQuery q
  toFrom (SelectQueryP q) = SubQuery q
  -- Otherwise use the SqlSetOperation
  toFrom q = SqlSetOperation q

instance (ToFrom a, ToFromT a ~ a', ToFrom b, ToFromT b ~ b', ToMaybe b', mb ~ ToMaybeT b')
       => ToFrom (LeftOuterJoin a (b, (a' :& mb) -> SqlExpr (Value Bool))) where
  toFrom (LeftOuterJoin lhs (rhs, on')) = LeftJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a, ToFromT a ~ a', ToFrom b, ToFromT b ~ b', ToMaybe a', ma ~ ToMaybeT a', ToMaybe b', mb ~ ToMaybeT b')
       => ToFrom (FullOuterJoin a (b, (ma :& mb) -> SqlExpr (Value Bool))) where
  toFrom (FullOuterJoin lhs (rhs, on')) = FullJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a, ToFromT a ~ a', ToFrom b, ToFromT b ~ b', ToMaybe a', ma ~ ToMaybeT a')
       => ToFrom (RightOuterJoin a (b, (ma :& b') -> SqlExpr (Value Bool))) where
  toFrom (RightOuterJoin lhs (rhs, on')) = RightJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a, ToFromT a ~ a', ToFrom b, ToFromT b ~ b') => ToFrom (InnerJoin a (b, (a' :& b') -> SqlExpr (Value Bool))) where
  toFrom (InnerJoin lhs (rhs, on')) = InnerJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a, ToFrom b) => ToFrom (CrossJoin a b) where
  toFrom (CrossJoin lhs rhs) = CrossJoinFrom (toFrom lhs) (toFrom rhs)

type family Nullable a where
  Nullable (Maybe a) = a
  Nullable a =  a

type family ToMaybeT a where
  ToMaybeT (SqlExpr (Maybe a)) = SqlExpr (Maybe a)
  ToMaybeT (SqlExpr (Entity a)) = SqlExpr (Maybe (Entity a))
  ToMaybeT (SqlExpr (Value a)) = SqlExpr (Value (Maybe (Nullable a)))
  ToMaybeT (a :& b) = (ToMaybeT a :& ToMaybeT b)
  ToMaybeT (a, b) = (ToMaybeT a, ToMaybeT b)
  ToMaybeT (a, b, c) = (ToMaybeT a, ToMaybeT b, ToMaybeT c)
  ToMaybeT (a, b, c, d) = (ToMaybeT a, ToMaybeT b, ToMaybeT c, ToMaybeT d)
  ToMaybeT (a, b, c, d, e) = (ToMaybeT a, ToMaybeT b, ToMaybeT c, ToMaybeT d, ToMaybeT e)
  ToMaybeT (a, b, c, d, e, f) = (ToMaybeT a, ToMaybeT b, ToMaybeT c, ToMaybeT d, ToMaybeT e, ToMaybeT f)
  ToMaybeT (a, b, c, d, e, f, g) = (ToMaybeT a, ToMaybeT b, ToMaybeT c, ToMaybeT d, ToMaybeT e, ToMaybeT f, ToMaybeT g)
  ToMaybeT (a, b, c, d, e, f, g, h) = (ToMaybeT a, ToMaybeT b, ToMaybeT c, ToMaybeT d, ToMaybeT e, ToMaybeT f, ToMaybeT g, ToMaybeT h)

class ToMaybe a where
  toMaybe :: a -> ToMaybeT a

instance ToMaybe (SqlExpr (Maybe a)) where
  toMaybe = id

instance ToMaybe (SqlExpr (Entity a)) where
  toMaybe = EMaybe

instance ToMaybe (SqlExpr (Value a)) where
  toMaybe = veryUnsafeCoerceSqlExprValue

instance (ToMaybe a, ToMaybe b) => ToMaybe (a :& b) where
  toMaybe (a :& b) = (toMaybe a :& toMaybe b)

instance (ToMaybe a, ToMaybe b) => ToMaybe (a,b) where
  toMaybe (a, b) = (toMaybe a, toMaybe b)

instance ( ToMaybe a
         , ToMaybe b
         , ToMaybe c
         ) => ToMaybe (a,b,c) where
  toMaybe = to3 . toMaybe . from3

instance ( ToMaybe a
         , ToMaybe b
         , ToMaybe c
         , ToMaybe d
         ) => ToMaybe (a,b,c,d) where
  toMaybe = to4 . toMaybe . from4

instance ( ToMaybe a
         , ToMaybe b
         , ToMaybe c
         , ToMaybe d
         , ToMaybe e
         ) => ToMaybe (a,b,c,d,e) where
  toMaybe = to5 . toMaybe . from5

instance ( ToMaybe a
         , ToMaybe b
         , ToMaybe c
         , ToMaybe d
         , ToMaybe e
         , ToMaybe f
         ) => ToMaybe (a,b,c,d,e,f) where
  toMaybe = to6 . toMaybe . from6

instance ( ToMaybe a
         , ToMaybe b
         , ToMaybe c
         , ToMaybe d
         , ToMaybe e
         , ToMaybe f
         , ToMaybe g
         ) => ToMaybe (a,b,c,d,e,f,g) where
  toMaybe = to7 . toMaybe . from7

instance ( ToMaybe a
         , ToMaybe b
         , ToMaybe c
         , ToMaybe d
         , ToMaybe e
         , ToMaybe f
         , ToMaybe g
         , ToMaybe h
         ) => ToMaybe (a,b,c,d,e,f,g,h) where
  toMaybe = to8 . toMaybe . from8

-- | 'FROM' clause, used to bring entities into scope.
--
-- Internally, this function uses the `From` datatype and the
-- `ToFrom` typeclass. Unlike the old `Database.Esqueleto.from`,
-- this does not take a function as a parameter, but rather
-- a value that represents a 'JOIN' tree constructed out of
-- instances of `ToFrom`. This implementation eliminates certain
-- types of runtime errors by preventing the construction of
-- invalid SQL (e.g. illegal nested-@from@).
from :: ToFrom a  => a -> SqlQuery (ToFromT a)
from parts = do
  (a, clause) <- runFrom $ toFrom parts
  Q $ W.tell mempty{sdFromClause=[clause]}
  pure a
    where
      runFrom :: From a -> SqlQuery (a, FromClause)
      runFrom e@Table = do
        let ed = entityDef $ getVal e
        ident <- newIdentFor (entityDB ed)
        let entity = EEntity ident
        pure $ (entity, FromStart ident ed)
          where
            getVal :: PersistEntity ent => From (SqlExpr (Entity ent)) -> Proxy ent
            getVal = const Proxy
      runFrom (SubQuery subquery) = do
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
          pure (ref , FromQuery subqueryAlias (\info -> toRawSql SELECT info aliasedQuery))

      runFrom (SqlSetOperation operation) = do
          (aliasedOperation, ret) <- aliasQueries operation
          ident <- newIdentFor (DBName "u")
          ref <- toAliasReference ident ret
          pure (ref, FromQuery ident $ operationToSql aliasedOperation)

          where
            aliasQueries o =
              case o of
                SelectQuery q -> do
                  (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ q
                  prevState <- Q $ lift S.get
                  aliasedRet <- toAlias ret
                  Q $ lift $ S.put prevState
                  let selectConstructor = if    (sdLimitClause sideData) /= mempty 
                                             || length (sdOrderByClause sideData) > 0 then
                                            SelectQueryP
                                          else 
                                            SelectQuery
                  pure (selectConstructor $ Q $ W.WriterT $ pure (aliasedRet, sideData), aliasedRet)
                SelectQueryP q -> do
                  (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ q
                  prevState <- Q $ lift S.get
                  aliasedRet <- toAlias ret
                  Q $ lift $ S.put prevState
                  pure (SelectQueryP $ Q $ W.WriterT $ pure (aliasedRet, sideData), aliasedRet)
                Union     o1 o2 -> do
                  (o1', ret) <- aliasQueries o1
                  (o2', _  ) <- aliasQueries o2
                  pure (Union o1' o2', ret)
                UnionAll  o1 o2 -> do
                  (o1', ret) <- aliasQueries o1
                  (o2', _  ) <- aliasQueries o2
                  pure (UnionAll o1' o2', ret)
                Except    o1 o2 -> do
                  (o1', ret) <- aliasQueries o1
                  (o2', _  ) <- aliasQueries o2
                  pure (Except o1' o2', ret)
                Intersect o1 o2 -> do
                  (o1', ret) <- aliasQueries o1
                  (o2', _  ) <- aliasQueries o2
                  pure (Intersect o1' o2', ret)

            operationToSql o info =
              case o of
                SelectQuery q   -> toRawSql SELECT info q
                SelectQueryP q  -> 
                  let (builder, values) = toRawSql SELECT info q
                  in (parens builder, values)
                Union     o1 o2 -> doSetOperation "UNION"     info o1 o2
                UnionAll  o1 o2 -> doSetOperation "UNION ALL" info o1 o2
                Except    o1 o2 -> doSetOperation "EXCEPT"    info o1 o2
                Intersect o1 o2 -> doSetOperation "INTERSECT" info o1 o2

            doSetOperation operationText info o1 o2 =
                  let
                    (q1, v1) = operationToSql o1 info
                    (q2, v2) = operationToSql o2 info
                  in (q1 <> " " <> operationText <> " " <> q2, v1 <> v2)


      runFrom (InnerJoinFrom leftPart (rightPart, on')) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = leftVal :& rightVal
        pure $ (ret, FromJoin leftFrom InnerJoinKind rightFrom (Just (on' ret)))
      runFrom (CrossJoinFrom leftPart rightPart) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = leftVal :& rightVal
        pure $ (ret, FromJoin leftFrom CrossJoinKind rightFrom Nothing)
      runFrom (LeftJoinFrom leftPart (rightPart, on')) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = leftVal :& (toMaybe rightVal)
        pure $ (ret, FromJoin leftFrom LeftOuterJoinKind rightFrom (Just (on' ret)))
      runFrom (RightJoinFrom leftPart (rightPart, on')) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = (toMaybe leftVal) :& rightVal
        pure $ (ret, FromJoin leftFrom RightOuterJoinKind rightFrom (Just (on' ret)))
      runFrom (FullJoinFrom leftPart (rightPart, on')) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = (toMaybe leftVal) :& (toMaybe rightVal)
        pure $ (ret, FromJoin leftFrom FullOuterJoinKind rightFrom (Just (on' ret)))

type family ToAliasT a where
  ToAliasT (SqlExpr (Value a)) = SqlExpr (Value a)
  ToAliasT (SqlExpr (Entity a)) = SqlExpr (Entity a)
  ToAliasT (a, b) = (ToAliasT a, ToAliasT b)
  ToAliasT (a, b, c) = (ToAliasT a, ToAliasT b, ToAliasT c)
  ToAliasT (a, b, c, d) = (ToAliasT a, ToAliasT b, ToAliasT c, ToAliasT d)
  ToAliasT (a, b, c, d, e) = (ToAliasT a, ToAliasT b, ToAliasT c, ToAliasT d, ToAliasT e)
  ToAliasT (a, b, c, d, e, f) = (ToAliasT a, ToAliasT b, ToAliasT c, ToAliasT d, ToAliasT e, ToAliasT f)
  ToAliasT (a, b, c, d, e, f, g) = (ToAliasT a, ToAliasT b, ToAliasT c, ToAliasT d, ToAliasT e, ToAliasT f, ToAliasT g)
  ToAliasT (a, b, c, d, e, f, g, h) = (ToAliasT a, ToAliasT b, ToAliasT c, ToAliasT d, ToAliasT e, ToAliasT f, ToAliasT g, ToAliasT h)

-- Tedious tuple magic
class ToAlias a where
  toAlias :: a -> SqlQuery (ToAliasT a)

instance ToAlias (SqlExpr (Value a)) where
  toAlias v@(EAliasedValue _ _) = pure v
  toAlias v = do
    ident <- newIdentFor (DBName "v")
    pure $ EAliasedValue ident v

instance ToAlias (SqlExpr (Entity a)) where
  toAlias v@(EAliasedEntityReference _ _) = pure v
  toAlias v@(EAliasedEntity _ _) = pure v
  toAlias (EEntity tableIdent) = do
    ident <- newIdentFor (DBName "v")
    pure $ EAliasedEntity ident tableIdent

instance (ToAlias a, ToAlias b) => ToAlias (a,b) where
  toAlias (a,b) = (,) <$> toAlias a <*> toAlias b

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         ) => ToAlias (a,b,c) where
  toAlias x = to3 <$> (toAlias $ from3 x)

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         , ToAlias d
         ) => ToAlias (a,b,c,d) where
  toAlias x = to4 <$> (toAlias $ from4 x)

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         , ToAlias d
         , ToAlias e
         ) => ToAlias (a,b,c,d,e) where
  toAlias x = to5 <$> (toAlias $ from5 x)

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         , ToAlias d
         , ToAlias e
         , ToAlias f
         ) => ToAlias (a,b,c,d,e,f) where
  toAlias x = to6 <$> (toAlias $ from6 x)

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         , ToAlias d
         , ToAlias e
         , ToAlias f
         , ToAlias g
         ) => ToAlias (a,b,c,d,e,f,g) where
  toAlias x = to7 <$> (toAlias $ from7 x)

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         , ToAlias d
         , ToAlias e
         , ToAlias f
         , ToAlias g
         , ToAlias h
         ) => ToAlias (a,b,c,d,e,f,g,h) where
  toAlias x = to8 <$> (toAlias $ from8 x)


type family ToAliasReferenceT a where
  ToAliasReferenceT (SqlExpr (Value a)) = SqlExpr (Value a)
  ToAliasReferenceT (SqlExpr (Entity a)) = SqlExpr (Entity a)
  ToAliasReferenceT (a,b) = (ToAliasReferenceT a, ToAliasReferenceT b)
  ToAliasReferenceT (a,b,c) = (ToAliasReferenceT a, ToAliasReferenceT b, ToAliasReferenceT c)
  ToAliasReferenceT (a, b, c, d) = (ToAliasReferenceT a, ToAliasReferenceT b, ToAliasReferenceT c, ToAliasReferenceT d)
  ToAliasReferenceT (a, b, c, d, e) = (ToAliasReferenceT a, ToAliasReferenceT b, ToAliasReferenceT c, ToAliasReferenceT d, ToAliasReferenceT e)
  ToAliasReferenceT (a, b, c, d, e, f) = (ToAliasReferenceT a, ToAliasReferenceT b, ToAliasReferenceT c, ToAliasReferenceT d, ToAliasReferenceT e, ToAliasReferenceT f)
  ToAliasReferenceT (a, b, c, d, e, f, g) = (ToAliasReferenceT a, ToAliasReferenceT b, ToAliasReferenceT c, ToAliasReferenceT d, ToAliasReferenceT e, ToAliasReferenceT f, ToAliasReferenceT g)
  ToAliasReferenceT (a, b, c, d, e, f, g, h) = (ToAliasReferenceT a, ToAliasReferenceT b, ToAliasReferenceT c, ToAliasReferenceT d, ToAliasReferenceT e, ToAliasReferenceT f, ToAliasReferenceT g, ToAliasReferenceT h)

-- more tedious tuple magic
class ToAliasReference a where
  toAliasReference :: Ident -> a -> SqlQuery (ToAliasReferenceT a)

instance ToAliasReference (SqlExpr (Value a)) where
  toAliasReference aliasSource (EAliasedValue aliasIdent _) = pure $ EValueReference aliasSource (\_ -> aliasIdent)
  toAliasReference _           v@(ERaw _ _)                 = toAlias v
  toAliasReference _           v@(ECompositeKey _)          = toAlias v
  toAliasReference _           v@(EValueReference _ _)      = pure v

instance ToAliasReference (SqlExpr (Entity a)) where
  toAliasReference aliasSource (EAliasedEntity ident _) = pure $ EAliasedEntityReference aliasSource ident
  toAliasReference _ e@(EEntity _) = toAlias e
  toAliasReference _ e@(EAliasedEntityReference _ _) = pure e

instance (ToAliasReference a, ToAliasReference b) => ToAliasReference (a, b) where
  toAliasReference ident (a,b) = (,) <$> (toAliasReference ident a) <*> (toAliasReference ident b)

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         ) => ToAliasReference (a,b,c) where
  toAliasReference ident x = fmap to3 $ toAliasReference ident $ from3 x

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         , ToAliasReference d
         ) => ToAliasReference (a,b,c,d) where
  toAliasReference ident x = fmap to4 $ toAliasReference ident $ from4 x

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         , ToAliasReference d
         , ToAliasReference e
         ) => ToAliasReference (a,b,c,d,e) where
  toAliasReference ident x = fmap to5 $ toAliasReference ident $ from5 x

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         , ToAliasReference d
         , ToAliasReference e
         , ToAliasReference f
         ) => ToAliasReference (a,b,c,d,e,f) where
  toAliasReference ident x = to6 <$> (toAliasReference ident $ from6 x)

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         , ToAliasReference d
         , ToAliasReference e
         , ToAliasReference f
         , ToAliasReference g
         ) => ToAliasReference (a,b,c,d,e,f,g) where
  toAliasReference ident x = to7 <$> (toAliasReference ident $ from7 x)

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         , ToAliasReference d
         , ToAliasReference e
         , ToAliasReference f
         , ToAliasReference g
         , ToAliasReference h
         ) => ToAliasReference (a,b,c,d,e,f,g,h) where
  toAliasReference ident x = to8 <$> (toAliasReference ident $ from8 x)
