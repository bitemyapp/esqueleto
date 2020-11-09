{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.From.Join
    where

import Data.Kind (Constraint)
import Data.Proxy
import Database.Esqueleto.Experimental.From
import Database.Esqueleto.Experimental.From.SqlSetOperation
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Experimental.ToMaybe
import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)
import Database.Esqueleto.Internal.PersistentImport (Entity(..))
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

data Lateral
data NotLateral

type family IsLateral a where
    IsLateral (a -> SqlQuery b) = Lateral
    IsLateral a = NotLateral

type family ErrorOnLateral a :: Constraint where
  ErrorOnLateral (a -> SqlQuery b) = TypeError ('Text "LATERAL can only be used for INNER, LEFT, and CROSS join kinds.")
  ErrorOnLateral _ = ()

{-- Type class magic to allow the use of the `InnerJoin` family of data constructors in from --}

type family FromOnClause a where
    FromOnClause (a, b -> SqlExpr (Value Bool)) = b
    FromOnClause a = TypeError ('Text "Missing ON clause")

instance {-# OVERLAPPABLE #-} From (InnerJoin a b) where
    type FromT (InnerJoin a b) = FromOnClause b
    runFrom = undefined
instance {-# OVERLAPPABLE #-} From (LeftOuterJoin a b) where
    type FromT (LeftOuterJoin a b) = FromOnClause b
    runFrom = undefined
instance {-# OVERLAPPABLE #-} From (RightOuterJoin a b) where
    type FromT (RightOuterJoin a b) = FromOnClause b
    runFrom = undefined
instance {-# OVERLAPPABLE #-} From (FullOuterJoin a b) where
    type FromT (FullOuterJoin a b) = FromOnClause b
    runFrom = undefined

class FromInnerJoin lateral lhs rhs res where
    runFromInnerJoin :: Proxy lateral -> lhs -> rhs -> (res -> SqlExpr (Value Bool)) -> SqlQuery (res, FromClause)

instance ( SqlSelect b r
         , ToAlias b
         , ToAliasReference b
         , From a
         , FromT a ~ a'
         ) => FromInnerJoin Lateral a (a' -> SqlQuery b) (a' :& b) where
             runFromInnerJoin _ leftPart q on' = do
                 (leftVal, leftFrom) <- runFrom leftPart
                 (rightVal, rightFrom) <- fromSubQuery LateralSubQuery (q leftVal)
                 let ret = leftVal :& rightVal
                 pure $ (ret, FromJoin leftFrom InnerJoinKind rightFrom (Just (on' ret)))

instance (From a, FromT a ~ a', From b, FromT b ~ b')
  => FromInnerJoin NotLateral a b (a' :& b') where
      runFromInnerJoin _ leftPart rightPart on' = do
          (leftVal, leftFrom) <- runFrom leftPart
          (rightVal, rightFrom) <- runFrom rightPart
          let ret = leftVal :& rightVal
          pure $ (ret, FromJoin leftFrom InnerJoinKind rightFrom (Just (on' ret)))

instance (FromInnerJoin (IsLateral b) a b b') => From (InnerJoin a (b, b' -> SqlExpr (Value Bool))) where
    type FromT (InnerJoin a (b, b' -> SqlExpr (Value Bool))) = FromOnClause (b, b' -> SqlExpr(Value Bool))
    runFrom (InnerJoin lhs (rhs, on')) = runFromInnerJoin (toProxy rhs) lhs rhs on'
        where
            toProxy :: b -> Proxy (IsLateral b)
            toProxy _ = Proxy

type family FromCrossJoin a b where
    FromCrossJoin a (b -> SqlQuery c) = FromT a :& c
    FromCrossJoin a b = FromT a :& FromT b

instance ( From a
         , From b
         , FromT (CrossJoin a b) ~ (FromT a :& FromT b)
         ) => From (CrossJoin a b) where
    type FromT (CrossJoin a b) = FromCrossJoin a b
    runFrom (CrossJoin leftPart rightPart) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = leftVal :& rightVal
        pure $ (ret, FromJoin leftFrom CrossJoinKind rightFrom Nothing)

instance {-# OVERLAPPING #-}
         ( From a
         , FromT a ~ a'
         , SqlSelect b r
         , ToAlias b
         , ToAliasReference b
         ) => From (CrossJoin a (a' -> SqlQuery b)) where
    type FromT (CrossJoin a (a' -> SqlQuery b)) = FromCrossJoin a (a' -> SqlQuery b)
    runFrom (CrossJoin leftPart q) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- fromSubQuery LateralSubQuery (q leftVal)
        let ret = leftVal :& rightVal
        pure $ (ret, FromJoin leftFrom CrossJoinKind rightFrom Nothing)

class FromLeftJoin lateral lhs rhs res where
    runFromLeftJoin :: Proxy lateral -> lhs -> rhs -> (res -> SqlExpr (Value Bool)) -> SqlQuery (res, FromClause)

instance ( From a
         , FromT a ~ a'
         , SqlSelect b r
         , ToAlias b
         , ToAliasReference b
         , ToMaybe b
         , mb ~ ToMaybeT b
         ) => FromLeftJoin Lateral a (a' -> SqlQuery b) (a' :& mb) where
            runFromLeftJoin _ leftPart q on' = do
                (leftVal, leftFrom) <- runFrom leftPart
                (rightVal, rightFrom) <- fromSubQuery LateralSubQuery (q leftVal)
                let ret = leftVal :& (toMaybe rightVal)
                pure $ (ret, FromJoin leftFrom LeftOuterJoinKind rightFrom (Just (on' ret)))

instance ( From a
         , FromT a ~ a'
         , From b
         , FromT b ~ b'
         , ToMaybe b'
         , mb ~ ToMaybeT b'
         ) => FromLeftJoin NotLateral a b (a' :& mb) where
            runFromLeftJoin _ leftPart rightPart on' = do
                (leftVal, leftFrom) <- runFrom leftPart
                (rightVal, rightFrom) <- runFrom rightPart
                let ret = leftVal :& (toMaybe rightVal)
                pure $ (ret, FromJoin leftFrom LeftOuterJoinKind rightFrom (Just (on' ret)))

instance ( FromLeftJoin (IsLateral b) a b b'
         ) => From (LeftOuterJoin a (b, b' -> SqlExpr (Value Bool))) where
            type FromT (LeftOuterJoin a (b, b' -> SqlExpr (Value Bool))) = FromOnClause (b, b' -> SqlExpr(Value Bool))
            runFrom (LeftOuterJoin lhs (rhs, on')) =
                runFromLeftJoin (toProxy rhs) lhs rhs on'
              where
                toProxy :: b -> Proxy (IsLateral b)
                toProxy _ = Proxy

instance ( From a
         , FromT a ~ a'
         , From b
         , FromT b ~ b'
         , ToMaybe a'
         , ma ~ ToMaybeT a'
         , ToMaybe b'
         , mb ~ ToMaybeT b'
         , ErrorOnLateral b
         ) => From (FullOuterJoin a (b, (ma :& mb) -> SqlExpr (Value Bool))) where
            type FromT (FullOuterJoin a (b, (ma :& mb) -> SqlExpr (Value Bool))) = FromOnClause (b, (ma :& mb) -> SqlExpr(Value Bool))
            runFrom (FullOuterJoin leftPart (rightPart, on')) = do
                (leftVal, leftFrom) <- runFrom leftPart
                (rightVal, rightFrom) <- runFrom rightPart
                let ret = (toMaybe leftVal) :& (toMaybe rightVal)
                pure $ (ret, FromJoin leftFrom FullOuterJoinKind rightFrom (Just (on' ret)))

instance ( From a
         , FromT a ~ a'
         , ToMaybe a'
         , ma ~ ToMaybeT a'
         , From b
         , FromT b ~ b'
         , ErrorOnLateral b
         ) => From (RightOuterJoin a (b, (ma :& b') -> SqlExpr (Value Bool))) where
            type FromT (RightOuterJoin a (b, (ma :& b') -> SqlExpr (Value Bool))) = FromOnClause (b, (ma :& b') -> SqlExpr(Value Bool))
            runFrom (RightOuterJoin leftPart (rightPart, on')) = do
                (leftVal, leftFrom) <- runFrom leftPart
                (rightVal, rightFrom) <- runFrom rightPart
                let ret = (toMaybe leftVal) :& rightVal
                pure $ (ret, FromJoin leftFrom RightOuterJoinKind rightFrom (Just (on' ret)))

instance (ToMaybe a, ToMaybe b) => ToMaybe (a :& b) where
    type ToMaybeT (a :& b) = (ToMaybeT a :& ToMaybeT b)
    toMaybe (a :& b) = (toMaybe a :& toMaybe b)
