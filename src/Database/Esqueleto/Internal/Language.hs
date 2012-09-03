{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies #-}
module Database.Esqueleto.Internal.Language
  ( Esqueleto(..)
  , from
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Database.Persist.GenericSql
import Database.Persist.Store


-- | Finally tagless representation of Esqueleto's EDSL.
class (Functor query, Applicative query, Monad query) =>
      Esqueleto query expr backend | query -> expr backend, expr -> query backend where
  -- | Single entity version of 'from'.
  fromSingle :: ( PersistEntity val
                , PersistEntityBackend val ~ backend)
             => query (expr (Entity val))

  -- | @WHERE@ clause: restrict the query's result.
  where_ :: expr (Single Bool) -> query ()

  -- | Execute a subquery in an expression.
  sub  :: query (expr a) -> expr a

  -- | Project a field of an entity.
  (^.) :: (PersistEntity val, PersistField typ) =>
          expr (Entity val) -> EntityField val typ -> expr (Single typ)

  -- | Lift a constant value from Haskell-land to the query.
  val  :: PersistField typ => typ -> expr (Single typ)

  not_ :: expr (Single Bool) -> expr (Single Bool)

  (==.) :: PersistField typ => expr (Single typ) -> expr (Single typ) -> expr (Single Bool)
  (>=.) :: PersistField typ => expr (Single typ) -> expr (Single typ) -> expr (Single Bool)
  (>.)  :: PersistField typ => expr (Single typ) -> expr (Single typ) -> expr (Single Bool)
  (<=.) :: PersistField typ => expr (Single typ) -> expr (Single typ) -> expr (Single Bool)
  (<.)  :: PersistField typ => expr (Single typ) -> expr (Single typ) -> expr (Single Bool)
  (!=.) :: PersistField typ => expr (Single typ) -> expr (Single typ) -> expr (Single Bool)

  (&&.) :: expr (Single Bool) -> expr (Single Bool) -> expr (Single Bool)
  (||.) :: expr (Single Bool) -> expr (Single Bool) -> expr (Single Bool)

  (+.)  :: (Num a, PersistField a) => expr (Single a) -> expr (Single a) -> expr (Single a)
  (-.)  :: (Num a, PersistField a) => expr (Single a) -> expr (Single a) -> expr (Single a)
  (/.)  :: (Num a, PersistField a) => expr (Single a) -> expr (Single a) -> expr (Single a)
  (*.)  :: (Num a, PersistField a) => expr (Single a) -> expr (Single a) -> expr (Single a)

-- Fixity declarations
infixl 9 ^.
infixl 7 *., /.
infixl 6 +., -.
infix  4 ==., >=., >., <=., <., !=.
infixr 3 &&.
infixr 2 ||.


class Esqueleto query expr backend => From query expr backend a where
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
  from :: query a

instance ( Esqueleto query expr backend
         , PersistEntity val
         , PersistEntityBackend val ~ backend
         ) => From query expr backend (expr (Entity val)) where
  from = fromSingle

instance ( From query expr backend a
         , From query expr backend b
         ) => From query expr backend (a, b) where
  from = (,) <$> from <*> from

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         ) => From query expr backend (a, b, c) where
  from = (,,) <$> from <*> from <*> from

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         , From query expr backend d
         ) => From query expr backend (a, b, c, d) where
  from = (,,,) <$> from <*> from <*> from <*> from

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         , From query expr backend d
         , From query expr backend e
         ) => From query expr backend (a, b, c, d, e) where
  from = (,,,,) <$> from <*> from <*> from <*> from <*> from

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         , From query expr backend d
         , From query expr backend e
         , From query expr backend f
         ) => From query expr backend (a, b, c, d, e, f) where
  from = (,,,,,) <$> from <*> from <*> from <*> from <*> from <*> from

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         , From query expr backend d
         , From query expr backend e
         , From query expr backend f
         , From query expr backend g
         ) => From query expr backend (a, b, c, d, e, f, g) where
  from = (,,,,,,) <$> from <*> from <*> from <*> from <*> from <*> from <*> from

instance ( From query expr backend a
         , From query expr backend b
         , From query expr backend c
         , From query expr backend d
         , From query expr backend e
         , From query expr backend f
         , From query expr backend g
         , From query expr backend h
         ) => From query expr backend (a, b, c, d, e, f, g, h) where
  from = (,,,,,,,) <$> from <*> from <*> from <*> from <*> from <*> from <*> from <*> from
