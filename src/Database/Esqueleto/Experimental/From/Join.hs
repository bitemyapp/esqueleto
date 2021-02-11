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

-- | Constraint for `on`. Ensures that only types that require an `on` can be used on
-- the left hand side. This was previously reusing the From class which was actually
-- a bit too lenient as it allowed to much.
--
-- @since 3.4.0.0
type family ValidOnClauseValue a :: Constraint where
  ValidOnClauseValue (Table a) = ()
  ValidOnClauseValue (SubQuery a) = ()
  ValidOnClauseValue (SqlQuery a) = ()
  ValidOnClauseValue (SqlSetOperation a) = ()
  ValidOnClauseValue (a -> SqlQuery b) = ()
  ValidOnClauseValue (From a) = ()
  ValidOnClauseValue _ = TypeError ('Text "Illegal use of ON")

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
on :: ValidOnClauseValue a => a -> (b -> SqlExpr (Value Bool)) -> (a, b -> SqlExpr (Value Bool))
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

crossJoin :: ( ToFrom a a'
             , ToFrom b b'
             ) => a -> b -> From (a' :& b')
crossJoin lhs rhs = From $ do
     (leftVal, leftFrom) <- unFrom (toFrom lhs)
     (rightVal, rightFrom) <- unFrom (toFrom rhs)
     let ret = leftVal :& rightVal
     pure $ (ret, fromJoin " CROSS JOIN " leftFrom rightFrom Nothing)

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

