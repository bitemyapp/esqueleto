{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.From.Join
    where

import Data.Bifunctor (first)
import Data.Kind (Constraint)
import Data.Proxy
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Experimental.From
import Database.Esqueleto.Experimental.From.SqlSetOperation
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Experimental.ToMaybe
import Database.Esqueleto.Internal.Internal hiding
       (From(..), from, fromJoin, on)
import Database.Esqueleto.Internal.PersistentImport
       (Entity(..), EntityField, PersistEntity, PersistField)
import GHC.TypeLits

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

instance (ToMaybe a, ToMaybe b) => ToMaybe (a :& b) where
    type ToMaybeT (a :& b) = (ToMaybeT a :& ToMaybeT b)
    toMaybe (a :& b) = (toMaybe a :& toMaybe b)

class ValidOnClause a
instance {-# OVERLAPPABLE #-} ToFrom a a' => ValidOnClause a
instance ValidOnClause (a -> SqlQuery b)

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
on :: ValidOnClause a => a -> (b -> SqlExpr Bool) -> (a, b -> SqlExpr Bool)
on = (,)
infix 9 `on`

type family ErrorOnLateral a :: Constraint where
  ErrorOnLateral (a -> SqlQuery b) = TypeError ('Text "LATERAL can only be used for INNER, LEFT, and CROSS join kinds.")
  ErrorOnLateral _ = ()

fromJoin :: TLB.Builder -> RawFn -> RawFn -> Maybe (SqlExpr Bool) -> RawFn
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
    HasOnClause (a, b -> SqlExpr Bool) c = () -- Let the compiler handle the type mismatch
    HasOnClause a expected =
        TypeError ( 'Text "Missing ON clause for join with"
                    ':$$: 'ShowType a
                    ':$$: 'Text ""
                    ':$$: 'Text "Expected: "
                    ':$$: 'ShowType a
                    ':$$: 'Text "`on` " ':<>: 'ShowType (expected -> SqlExpr Bool)
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
-- /Since: 3.5.0.0/
innerJoin :: ( ToFrom a a'
             , ToFrom b b'
             , HasOnClause rhs (a' :& b')
             , rhs ~ (b, (a' :& b') -> SqlExpr Bool)
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
-- /Since: 3.5.0.0/
innerJoinLateral :: ( ToFrom a a'
                    , HasOnClause rhs (a' :& b)
                    , SqlSelect b r
                    , ToAlias b
                    , ToAliasReference b
                    , rhs ~ (a' -> SqlQuery b, (a' :& b) -> SqlExpr Bool)
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
-- /Since: 3.5.0.0/
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
-- /Since: 3.5.0.0/
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
--         p ^. PersonId ==. bp ?. BlogPostAuthorId)
-- @
--
-- /Since: 3.5.0.0/
leftJoin :: ( ToFrom a a'
            , ToFrom b b'
            , ToMaybe b'
            , HasOnClause rhs (a' :& ToMaybeT b')
            , rhs ~ (b, (a' :& ToMaybeT b') -> SqlExpr Bool)
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
-- /Since: 3.5.0.0/
leftJoinLateral :: ( ToFrom a a'
                   , SqlSelect b r
                   , HasOnClause rhs (a' :& ToMaybeT b)
                   , ToAlias b
                   , ToAliasReference b
                   , ToMaybe b
                   , rhs ~ (a' -> SqlQuery b, (a' :& ToMaybeT b) -> SqlExpr Bool)
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
-- /Since: 3.5.0.0/
rightJoin :: ( ToFrom a a'
             , ToFrom b b'
             , ToMaybe a'
             , HasOnClause rhs (ToMaybeT a' :& b')
             , rhs ~ (b, (ToMaybeT a' :& b') -> SqlExpr Bool)
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
-- /Since: 3.5.0.0/
fullOuterJoin :: ( ToFrom a a'
                 , ToFrom b b'
                 , ToMaybe a'
                 , ToMaybe b'
                 , HasOnClause rhs (ToMaybeT a' :& ToMaybeT b')
                 , rhs ~ (b, (ToMaybeT a' :& ToMaybeT b') -> SqlExpr Bool)
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
         , rhs ~ (b, (a' :& b') -> SqlExpr Bool)
         ) => DoInnerJoin NotLateral a rhs (a' :& b') where
    doInnerJoin _ = innerJoin

instance ( ToFrom a a'
         , SqlSelect b r
         , ToAlias b
         , ToAliasReference b
         , d ~ (a' :& b)
         ) => DoInnerJoin Lateral a (a' -> SqlQuery b, d -> SqlExpr Bool) d where
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
         , rhs ~ (b, (a' :& mb) -> SqlExpr Bool)
         ) => DoLeftJoin NotLateral a rhs (a' :& mb) where
    doLeftJoin _ = leftJoin

instance ( ToFrom a a'
         , ToMaybe b
         , d ~ (a' :& ToMaybeT b)
         , SqlSelect b r
         , ToAlias b
         , ToAliasReference b
         ) => DoLeftJoin Lateral a (a' -> SqlQuery b, d -> SqlExpr Bool) d where
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
         , rhs ~ (b, (ma :& b') -> SqlExpr Bool)
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
         , rhs ~ (b, (ma :& mb) -> SqlExpr Bool)
         ) => ToFrom (FullOuterJoin a rhs) (ma :& mb) where
    toFrom (FullOuterJoin a b) = fullOuterJoin a b

