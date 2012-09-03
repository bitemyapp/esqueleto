{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs #-}
module Database.Esqueleto.Internal.Language
  ( -- * Constructing queries
    select
  , from
  , where_

    -- * Expressions
  , (^.)
  , val
  , sub

    -- ** Comparison operators
  , (==.)
  , (>=.)
  , (>.)
  , (<=.)
  , (<.)
  , (!=.)

    -- ** Boolean operators
  , not_
  , (&&.)
  , (||.)

    -- ** Numerical operators
  , (+.)
  , (-.)
  , (*.)
  , (/.)
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Database.Persist.GenericSql
import Database.Persist.Store
import Database.Persist.Query (PersistQuery)
import qualified Control.Monad.Supply as S

import Database.Esqueleto.Internal.Types


-- | TODO
select :: PersistQuery SqlPersist m => Query a -> SqlPersist m a
select = undefined


class From a where
  -- | @FROM@ clause: bring an entity into scope.
  --
  -- The following types implement 'from':
  --
  --  * @Expr (Entity val)@, which brings a single entity into scope.
  --
  --  * Tuples of any other types supported by 'from'.  Calling
  --  'from' multiple times is the same as calling 'from' a
  --  single time and using a tuple.
  --
  -- Note that using 'from' for the same entity twice does work
  -- and corresponds to a self-join.  You don't even need to use
  -- two different calls to 'from', you may use a tuple.
  from :: Query a

instance (PersistEntity val, PersistEntityBackend val ~ SqlPersist) => From (Expr (Entity val)) where
  from = Q $ ETable <$> S.supply
instance (From a, From b) => From (a, b) where
  from = (,) <$> from <*> from
instance (From a, From b, From c) => From (a, b, c) where
  from = (,,) <$> from <*> from <*> from
instance (From a, From b, From c, From d) => From (a, b, c, d) where
  from = (,,,) <$> from <*> from <*> from <*> from
instance (From a, From b, From c, From d, From e) => From (a, b, c, d, e) where
  from = (,,,,) <$> from <*> from <*> from <*> from <*> from
instance (From a, From b, From c, From d, From e, From f) => From (a, b, c, d, e, f) where
  from = (,,,,,) <$> from <*> from <*> from <*> from <*> from <*> from
instance (From a, From b, From c, From d, From e, From f, From g) => From (a, b, c, d, e, f, g) where
  from = (,,,,,,) <$> from <*> from <*> from <*> from <*> from <*> from <*> from
instance (From a, From b, From c, From d, From e, From f, From g, From h) => From (a, b, c, d, e, f, g, h) where
  from = (,,,,,,,) <$> from <*> from <*> from <*> from <*> from <*> from <*> from <*> from


-- | @WHERE@ clause: restrict the query's result.
where_ :: Value Bool -> Query ()
where_ = undefined


-- | Execute a subquery in an expression.
sub :: Query (Expr a) -> Expr a
sub = ESub


-- | Project a field of an entity.
(^.) :: PersistField typ => Expr (Entity val) -> EntityField val typ -> Value typ
(^.) = EProj


-- | Lift a constant value from Haskell-land to the query.
val :: PersistField typ => typ -> Value typ
val = EVal


(==.), (>=.), (>.), (<=.), (<.), (!=.) :: PersistField typ => Value typ -> Value typ -> Value Bool
(==.) = EBinOp OEq
(>=.) = EBinOp OGeq
(>.)  = EBinOp OGt
(<=.) = EBinOp OLeq
(<.)  = EBinOp OLt
(!=.) = EBinOp ONeq

not_ :: Value Bool -> Value Bool
not_ = ENot

(&&.), (||.) :: Value Bool -> Value Bool -> Value Bool
(&&.) = EBinOp OAnd
(||.) = EBinOp OOr

(+.), (-.), (/.), (*.) :: (Num a, PersistField a) => Value a -> Value a -> Value a
(+.) = EBinOp OAdd
(-.) = EBinOp OSub
(/.) = EBinOp ODiv
(*.) = EBinOp OMult


-- Fixity declarations
infixl 9 ^.
infixl 7 *., /.
infixl 6 +., -.
infix  4 ==., >=., >., <=., <., !=.
infixr 3 &&.
infixr 2 ||.
