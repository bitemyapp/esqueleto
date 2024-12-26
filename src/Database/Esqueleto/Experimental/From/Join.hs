{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Esqueleto.Experimental.From.Join
    ( (:&)(..)
    , ValidOnClause
    , on
    , ErrorOnLateral
    , fromJoin
    , HasOnClause
    , innerJoin
    , innerJoinLateral
    , crossJoin
    , crossJoinLateral
    , leftJoin
    , leftJoinLateral
    , rightJoin
    , fullOuterJoin
    , GetFirstTable(..)
    , getTable
    , getTableMaybe
    -- Compatability for old syntax
    , Lateral
    , NotLateral
    , IsLateral
    , DoInnerJoin(..)
    , DoLeftJoin(..)
    , DoCrossJoin(..)
    ) where

import Data.Bifunctor (first)
import Data.Kind (Constraint)
import Data.Proxy
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Experimental.From
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Experimental.ToMaybe
import Database.Esqueleto.Internal.Internal hiding
       (From(..), from, fromJoin, on)
import Database.Esqueleto.Internal.PersistentImport (Entity)
import GHC.TypeLits

instance (ToMaybe a, ToMaybe b) => ToMaybe (a :& b) where
    type ToMaybeT (a :& b) = (ToMaybeT a :& ToMaybeT b)
    toMaybe (a :& b) = (toMaybe a :& toMaybe b)

class ValidOnClause a
instance {-# OVERLAPPABLE #-} ToFrom a a' => ValidOnClause a
instance ValidOnClause (a -> SqlQuery b)

-- | You may return joined values from a 'select' query - this is
-- identical to the tuple instance, but is provided for convenience.
--
-- @since 3.5.2.0
instance (SqlSelect a ra, SqlSelect b rb) => SqlSelect (a :& b) (ra :& rb) where
    sqlSelectCols esc (a :& b) = sqlSelectCols esc (a, b)
    sqlSelectColCount = sqlSelectColCount . toTuple
      where
        toTuple :: Proxy (a :& b) -> Proxy (a, b)
        toTuple = const Proxy
    sqlSelectProcessRow = fmap (uncurry (:&)) . sqlSelectProcessRow

-- | Identical to the tuple instance and provided for convenience.
--
-- @since 3.5.3.0
instance (ToAlias a, ToAlias b) => ToAlias (a :& b) where
    toAlias (a :& b) = (:&) <$> toAlias a <*> toAlias b

-- | Identical to the tuple instance and provided for convenience.
--
-- @since 3.5.3.0
instance (ToAliasReference a, ToAliasReference b) => ToAliasReference (a :& b) where
    toAliasReference ident (a :& b) = (:&) <$> (toAliasReference ident a) <*> (toAliasReference ident b)

-- | An @ON@ clause that describes how two tables are related. This should be
-- used as an infix operator after a 'JOIN'. For example,
--
-- @
-- select $
-- from $ table \@Person
-- \`innerJoin\` table \@BlogPost
-- \`on\` (\\(p :& bP) ->
--         p ^. PersonId ==. bP ^. BlogPostAuthorId)
-- @
on :: ValidOnClause a => a -> (b -> SqlExpr (Value Bool)) -> (a, b -> SqlExpr (Value Bool))
on = (,)
infix 9 `on`

type family ErrorOnLateral a :: Constraint where
  ErrorOnLateral (a -> SqlQuery b) = TypeError ('Text "LATERAL can only be used for INNER, LEFT, and CROSS join kinds.")
  ErrorOnLateral _ = ()

fromJoin :: TLB.Builder -> RawFn -> RawFn -> Maybe (SqlExpr (Value Bool)) -> RawFn
fromJoin joinKind lhs rhs monClause =
    \paren info ->
        first (parensM paren) $
        mconcat [ lhs Never info
                , (joinKind, mempty)
                , rhs Parens info
                , maybe mempty (makeOnClause info) monClause
                ]
    where
        makeOnClause info (ERaw _ f)        = first (" ON " <>) (f Never info)

type family HasOnClause actual expected :: Constraint where
    HasOnClause (a, b -> SqlExpr (Value Bool)) c = () -- Let the compiler handle the type mismatch
    HasOnClause a expected =
        TypeError ( 'Text "Missing ON clause for join with"
                    ':$$: 'ShowType a
                    ':$$: 'Text ""
                    ':$$: 'Text "Expected: "
                    ':$$: 'ShowType a
                    ':$$: 'Text "`on` " ':<>: 'ShowType (expected -> SqlExpr (Value Bool))
                    ':$$: 'Text ""
                  )


-- | INNER JOIN
--
-- Used as an infix operator \`innerJoin\`
--
-- @
-- select $
-- from $ table \@Person
-- \`innerJoin\` table \@BlogPost
-- \`on\` (\\(p :& bp) ->
--         p ^. PersonId ==. bp ^. BlogPostAuthorId)
-- @
--
-- @since 3.5.0.0
innerJoin :: ( ToFrom a a'
             , ToFrom b b'
             , HasOnClause rhs (a' :& b')
             , rhs ~ (b, (a' :& b') -> SqlExpr (Value Bool))
             ) => a -> rhs -> From (a' :& b')
innerJoin lhs (rhs, on') = From $ do
     (leftVal, leftFrom) <- unFrom (toFrom lhs)
     (rightVal, rightFrom) <- unFrom (toFrom rhs)
     let ret = leftVal :& rightVal
     pure $ (ret, fromJoin " INNER JOIN " leftFrom rightFrom (Just $ on' ret))


-- | INNER JOIN LATERAL
--
-- A Lateral subquery join allows the joined query to reference entities from the
-- left hand side of the join. Discards rows that don't match the on clause
--
-- Used as an infix operator \`innerJoinLateral\`
--
-- See example 6
--
-- @since 3.5.0.0
innerJoinLateral :: ( ToFrom a a'
                    , HasOnClause rhs (a' :& b)
                    , SqlSelect b r
                    , ToAlias b
                    , ToAliasReference b
                    , rhs ~ (a' -> SqlQuery b, (a' :& b) -> SqlExpr (Value Bool))
                    )
                 => a -> rhs -> From (a' :& b)
innerJoinLateral lhs (rhsFn, on') = From $ do
     (leftVal, leftFrom) <- unFrom (toFrom lhs)
     (rightVal, rightFrom) <- unFrom (selectQuery (rhsFn leftVal))
     let ret = leftVal :& rightVal
     pure $ (ret, fromJoin " INNER JOIN LATERAL " leftFrom rightFrom (Just $ on' ret))

-- | CROSS JOIN
--
-- Used as an infix \`crossJoin\`
--
-- @
-- select $ do
-- from $ table \@Person
-- \`crossJoin\` table \@BlogPost
-- @
--
-- @since 3.5.0.0
crossJoin :: ( ToFrom a a'
             , ToFrom b b'
             ) => a -> b -> From (a' :& b')
crossJoin lhs rhs = From $ do
     (leftVal, leftFrom) <- unFrom (toFrom lhs)
     (rightVal, rightFrom) <- unFrom (toFrom rhs)
     let ret = leftVal :& rightVal
     pure $ (ret, fromJoin " CROSS JOIN " leftFrom rightFrom Nothing)

-- | CROSS JOIN LATERAL
--
-- A Lateral subquery join allows the joined query to reference entities from the
-- left hand side of the join.
--
-- Used as an infix operator \`crossJoinLateral\`
--
-- See example 6
--
-- @since 3.5.0.0
crossJoinLateral :: ( ToFrom a a'
                    , SqlSelect b r
                    , ToAlias b
                    , ToAliasReference b
                    )
                 => a -> (a' -> SqlQuery b) -> From (a' :& b)
crossJoinLateral lhs rhsFn = From $ do
     (leftVal, leftFrom) <- unFrom (toFrom lhs)
     (rightVal, rightFrom) <- unFrom (selectQuery (rhsFn leftVal))
     let ret = leftVal :& rightVal
     pure $ (ret, fromJoin " CROSS JOIN LATERAL " leftFrom rightFrom Nothing)

-- | LEFT OUTER JOIN
--
-- Join where the right side may not exist.
-- If the on clause fails then the right side will be NULL'ed
-- Because of this the right side needs to be handled as a Maybe
--
-- Used as an infix operator \`leftJoin\`
--
-- @
-- select $
-- from $ table \@Person
-- \`leftJoin\` table \@BlogPost
-- \`on\` (\\(p :& bp) ->
--         just (p ^. PersonId) ==. bp ?. BlogPostAuthorId)
-- @
--
-- @since 3.5.0.0
leftJoin :: ( ToFrom a a'
            , ToFrom b b'
            , ToMaybe b'
            , HasOnClause rhs (a' :& ToMaybeT b')
            , rhs ~ (b, (a' :& ToMaybeT b') -> SqlExpr (Value Bool))
            ) => a -> rhs -> From (a' :& ToMaybeT b')
leftJoin lhs (rhs, on') = From $ do
     (leftVal, leftFrom) <- unFrom (toFrom lhs)
     (rightVal, rightFrom) <- unFrom (toFrom rhs)
     let ret = leftVal :& toMaybe rightVal
     pure $ (ret, fromJoin " LEFT OUTER JOIN " leftFrom rightFrom (Just $ on' ret))

-- | LEFT OUTER JOIN LATERAL
--
-- Lateral join where the right side may not exist.
-- In the case that the query returns nothing or the on clause fails the right
-- side of the join will be NULL'ed
-- Because of this the right side needs to be handled as a Maybe
--
-- Used as an infix operator \`leftJoinLateral\`
--
-- See example 6 for how to use LATERAL
--
-- @since 3.5.0.0
leftJoinLateral :: ( ToFrom a a'
                   , SqlSelect b r
                   , HasOnClause rhs (a' :& ToMaybeT b)
                   , ToAlias b
                   , ToAliasReference b
                   , ToMaybe b
                   , rhs ~ (a' -> SqlQuery b, (a' :& ToMaybeT b) -> SqlExpr (Value Bool))
                   )
                 => a -> rhs -> From (a' :& ToMaybeT b)
leftJoinLateral lhs (rhsFn, on') = From $ do
     (leftVal, leftFrom) <- unFrom (toFrom lhs)
     (rightVal, rightFrom) <- unFrom (selectQuery (rhsFn leftVal))
     let ret = leftVal :& toMaybe rightVal
     pure $ (ret, fromJoin " LEFT OUTER JOIN LATERAL " leftFrom rightFrom (Just $ on' ret))

-- | RIGHT OUTER JOIN
--
-- Join where the left side may not exist.
-- If the on clause fails then the left side will be NULL'ed
-- Because of this the left side needs to be handled as a Maybe
--
-- Used as an infix operator \`rightJoin\`
--
-- @
-- select $
-- from $ table \@Person
-- \`rightJoin\` table \@BlogPost
-- \`on\` (\\(p :& bp) ->
--         p ?. PersonId ==. bp ^. BlogPostAuthorId)
-- @
--
-- @since 3.5.0.0
rightJoin :: ( ToFrom a a'
             , ToFrom b b'
             , ToMaybe a'
             , HasOnClause rhs (ToMaybeT a' :& b')
             , rhs ~ (b, (ToMaybeT a' :& b') -> SqlExpr (Value Bool))
             ) => a -> rhs -> From (ToMaybeT a' :& b')
rightJoin lhs (rhs, on') = From $ do
     (leftVal, leftFrom) <- unFrom (toFrom lhs)
     (rightVal, rightFrom) <- unFrom (toFrom rhs)
     let ret = toMaybe leftVal :& rightVal
     pure $ (ret, fromJoin " RIGHT OUTER JOIN " leftFrom rightFrom (Just $ on' ret))

-- | FULL OUTER JOIN
--
-- Join where both sides of the join may not exist.
-- Because of this the result needs to be handled as a Maybe
--
-- Used as an infix operator \`fullOuterJoin\`
--
-- @
-- select $
-- from $ table \@Person
-- \`fullOuterJoin\` table \@BlogPost
-- \`on\` (\\(p :& bp) ->
--         p ?. PersonId ==. bp ?. BlogPostAuthorId)
-- @
--
-- @since 3.5.0.0
fullOuterJoin :: ( ToFrom a a'
                 , ToFrom b b'
                 , ToMaybe a'
                 , ToMaybe b'
                 , HasOnClause rhs (ToMaybeT a' :& ToMaybeT b')
                 , rhs ~ (b, (ToMaybeT a' :& ToMaybeT b') -> SqlExpr (Value Bool))
                 ) => a -> rhs -> From (ToMaybeT a' :& ToMaybeT b')
fullOuterJoin lhs (rhs, on') = From $ do
     (leftVal, leftFrom) <- unFrom (toFrom lhs)
     (rightVal, rightFrom) <- unFrom (toFrom rhs)
     let ret = toMaybe leftVal :& toMaybe rightVal
     pure $ (ret, fromJoin " FULL OUTER JOIN " leftFrom rightFrom (Just $ on' ret))

infixl 2 `innerJoin`,
         `innerJoinLateral`,
         `leftJoin`,
         `leftJoinLateral`,
         `crossJoin`,
         `crossJoinLateral`,
         `rightJoin`,
         `fullOuterJoin`

-- | Typeclass for selecting tables using type application syntax.
--
-- If you have a long chain of tables joined with `(:&)`, like
-- @a :& b :& c :& d@, then @getTable \@c (a :& b :& c :& d)@ will give you the
-- @c@ table back.
--
-- Note that this typeclass will only select the first table of the given type;
-- it may be less useful if there's multiple tables of the same type.
--
-- @since 3.5.9.0
class GetFirstTable t ts where
  -- | Get the first table of type `t` from the tables `ts`.
  --
  -- @since 3.5.9.0
  getFirstTable :: ts -> t

instance GetFirstTable t (t :& ts) where
  getFirstTable (t :& _) = t

instance GetFirstTable t (x :& t) where
  getFirstTable (_ :& t) = t

-- The associativity of (:&) means we do the recursion along the left-hand side.
instance {-# OVERLAPPABLE #-} GetFirstTable t ts => GetFirstTable t (ts :& x) where
  getFirstTable (ts :& _) = getFirstTable ts

-- | Get the first table of a given type from a chain of tables joined with `(:&)`.
--
-- This can make it easier to write queries with a large number of join clauses:
--
-- @
-- select $ do
-- (people :& followers :& blogPosts) <-
--     from $ table \@Person
--     \`innerJoin` table \@Follow
--     \`on\` (\\(person :& follow) ->
--             person ^. PersonId ==. follow ^. FollowFollowed)
--     \`innerJoin` table \@BlogPost
--     \`on\` (\\((getTable \@Follow -> follow) :& blogPost) ->
--             blogPost ^. BlogPostAuthorId ==. follow ^. FollowFollower)
-- where_ (people1 ^. PersonName ==. val \"John\")
-- pure (followers, people2)
-- @
--
-- This example is a bit trivial, but once you've joined five or six tables it
-- becomes enormously helpful. The above example uses a @ViewPattern@ to call
-- the function and assign the variable directly, but you can also imagine it
-- being written like this:
--
-- @
--     \`on\` (\\(prev :& blogPost) ->
--             let
--                 follow = getTable \@Follow prev
--              in
--                 blogPost ^. BlogPostAuthorId ==. follow ^. FollowFollower)
-- @
--
-- This function will pluck out the first table that matches the applied type,
-- so if you join on the same table multiple times, it will always select the
-- first one provided.
--
-- The `(:&)` operator associates so that the left hand side can be a wildcard
-- for an arbitrary amount of nesting, and the "most recent" or "newest" table
-- in a join sequence is always available on the rightmost - so @(prev :& bar)@
-- is a pattern that matches @bar@ table (the most recent table added) and
-- @prev@ tables (all prior tables in the join match).
--
-- By calling 'getTable' on the @prev@, you can select exactly the table you
-- want, allowing you to omit a large number of spurious pattern matches.
-- Consider a query that does several @LEFT JOIN@ on a first table:
--
-- @
-- SELECT *
-- FROM person
-- LEFT JOIN car
--   ON person.id = car.person_id
-- LEFT JOIN bike
--   ON person.id = bike.person_id
-- LEFT JOIN food
--   ON person.id = food.person_id
-- LEFT JOIN address
--   ON person.id = address.person_id
-- @
--
-- The final 'on' clause in esqueleto would look like this:
--
-- @
--     \`on\` do
--         \\(person :& _car :& _bike :& _food :& address) ->
--             person.id ==. address.personId
-- @
--
-- First, we can change it to a @prev :& newest@ match. We can do this because
-- of the operator associativity. This is kind of like how a list @:@ operator
-- associates, but in the other direction: @a : (b : c) = a : b : c@.
--
-- @
--     \`on\` do
--         \\(prev :& address) ->
--             let (person :& _car :& _bike :& _food) = prev
--              in person.id ==. address.personId
-- @
--
-- Then, we can use 'getTable' to select the @Person@ table directly, instead of
-- pattern matching manually.
--
-- @
--     \`on\` do
--         \\(prev :& address) ->
--             let person = getTable \@Person prev
--              in person.id ==. address.personId
-- @
--
-- Finally, we can use a @ViewPattern@ language extension to "inline" the
-- access.
--
-- @
--     \`on\` do
--         \\((getTable \@Person -> person) :& address) ->
--            person.id ==. address.personId
-- @
--
-- With this form, you do not need to be concerned about the number and wildcard
-- status of tables that do not matter to the specific @ON@ clause.
--
-- @since 3.5.9.0
getTable :: forall t ts. GetFirstTable (SqlExpr (Entity t)) ts
         => ts
         -> SqlExpr (Entity t)
getTable = getFirstTable

-- | A variant of `getTable` that operates on possibly-null entities.
--
-- @since 3.5.9.0
getTableMaybe :: forall t ts. GetFirstTable (SqlExpr (Maybe (Entity t))) ts
              => ts
              -> SqlExpr (Maybe (Entity t))
getTableMaybe = getFirstTable

------ Compatibility for old syntax

data Lateral
data NotLateral

type family IsLateral a where
    IsLateral (a -> SqlQuery b, c) = Lateral
    IsLateral (a -> SqlQuery b) = Lateral
    IsLateral a = NotLateral

class DoInnerJoin lateral lhs rhs res | lateral rhs lhs -> res where
    doInnerJoin :: Proxy lateral -> lhs -> rhs -> From res

instance ( ToFrom a a'
         , ToFrom b b'
         , HasOnClause rhs (a' :& b')
         , rhs ~ (b, (a' :& b') -> SqlExpr (Value Bool))
         ) => DoInnerJoin NotLateral a rhs (a' :& b') where
    doInnerJoin _ = innerJoin

instance ( ToFrom a a'
         , SqlSelect b r
         , ToAlias b
         , ToAliasReference b
         , d ~ (a' :& b)
         ) => DoInnerJoin Lateral a (a' -> SqlQuery b, d -> SqlExpr (Value Bool)) d where
    doInnerJoin _ = innerJoinLateral

instance ( DoInnerJoin lateral lhs rhs r, lateral ~ IsLateral rhs )
           => ToFrom (InnerJoin lhs rhs) r where
     toFrom (InnerJoin a b) = doInnerJoin (Proxy @lateral) a b

class DoLeftJoin lateral lhs rhs res | lateral rhs lhs -> res where
    doLeftJoin :: Proxy lateral -> lhs -> rhs -> From res

instance ( ToFrom a a'
         , ToFrom b b'
         , ToMaybe b'
         , ToMaybeT b' ~ mb
         , HasOnClause rhs (a' :& mb)
         , rhs ~ (b, (a' :& mb) -> SqlExpr (Value Bool))
         ) => DoLeftJoin NotLateral a rhs (a' :& mb) where
    doLeftJoin _ = leftJoin

instance ( ToFrom a a'
         , ToMaybe b
         , d ~ (a' :& ToMaybeT b)
         , SqlSelect b r
         , ToAlias b
         , ToAliasReference b
         ) => DoLeftJoin Lateral a (a' -> SqlQuery b, d -> SqlExpr (Value Bool)) d where
    doLeftJoin _ = leftJoinLateral

instance ( DoLeftJoin lateral lhs rhs r, lateral ~ IsLateral rhs )
           => ToFrom (LeftOuterJoin lhs rhs) r where
     toFrom (LeftOuterJoin a b) = doLeftJoin (Proxy @lateral) a b

class DoCrossJoin lateral lhs rhs res | lateral lhs rhs -> res where
    doCrossJoin :: Proxy lateral -> lhs -> rhs -> From res

instance (ToFrom a a', ToFrom b b') => DoCrossJoin NotLateral a b (a' :& b') where
    doCrossJoin _ = crossJoin
instance (ToFrom a a', SqlSelect b r, ToAlias b, ToAliasReference b)
  => DoCrossJoin Lateral a (a' -> SqlQuery b) (a' :& b) where
    doCrossJoin _ = crossJoinLateral

instance (DoCrossJoin lateral lhs rhs r, IsLateral rhs ~ lateral)
  => ToFrom (CrossJoin lhs rhs) r where
    toFrom (CrossJoin a b) = doCrossJoin (Proxy @lateral) a b

instance ( ToFrom a a'
         , ToFrom b b'
         , ToMaybe a'
         , ToMaybeT a' ~ ma
         , HasOnClause rhs (ma :& b')
         , ErrorOnLateral b
         , rhs ~ (b, (ma :& b') -> SqlExpr (Value Bool))
         ) => ToFrom (RightOuterJoin a rhs) (ma :& b') where
    toFrom (RightOuterJoin a b) = rightJoin a b

instance ( ToFrom a a'
         , ToFrom b b'
         , ToMaybe a'
         , ToMaybeT a' ~ ma
         , ToMaybe b'
         , ToMaybeT b' ~ mb
         , HasOnClause rhs (ma :& mb)
         , ErrorOnLateral b
         , rhs ~ (b, (ma :& mb) -> SqlExpr (Value Bool))
         ) => ToFrom (FullOuterJoin a rhs) (ma :& mb) where
    toFrom (FullOuterJoin a b) = fullOuterJoin a b
