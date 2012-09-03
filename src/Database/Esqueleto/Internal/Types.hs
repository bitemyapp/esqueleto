{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs #-}
module Database.Esqueleto.Internal.Types
  ( Query(..)
  , Ident(..)
  , Expr(..)
  , BinOp(..)
  , Value
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (ap)
import Data.Text (Text)
import Database.Persist.GenericSql
import Database.Persist.Store
import qualified Control.Monad.Supply as S


-- | TODO
newtype Query a =
  Q { unQ :: S.Supply Ident a }

instance Functor Query where
  fmap f = Q . fmap f . unQ

instance Monad Query where
  return  = Q . return
  m >>= f = Q (unQ m >>= unQ . f)

instance Applicative Query where
  pure  = return
  (<*>) = ap


-- | Identifier used for tables.
newtype Ident = Ident Text


-- | TODO
data Expr a where
  ETable :: (PersistEntity val, PersistEntityBackend val ~ SqlPersist) => Ident -> Expr (Entity val)
  ESub   :: Query (Expr a) -> Expr a
  EProj  :: PersistField typ => Expr (Entity val) -> EntityField val typ -> Expr (Single typ)
  EVal   :: PersistField typ => typ -> Expr (Single typ)
  EBinOp :: BinOp a b r -> Expr (Single a) -> Expr (Single b) -> Expr (Single r)
  ENot   :: Expr (Single Bool) -> Expr (Single Bool)


-- | A binary operation.
data BinOp a b r where
  OEq  :: PersistField typ => BinOp typ typ Bool
  OGeq :: PersistField typ => BinOp typ typ Bool
  OGt  :: PersistField typ => BinOp typ typ Bool
  OLeq :: PersistField typ => BinOp typ typ Bool
  OLt  :: PersistField typ => BinOp typ typ Bool
  ONeq :: PersistField typ => BinOp typ typ Bool

  OAnd :: BinOp Bool Bool Bool
  OOr  :: BinOp Bool Bool Bool

  OAdd  :: (Num a, PersistField a) => BinOp a a a
  OSub  :: (Num a, PersistField a) => BinOp a a a
  ODiv  :: (Num a, PersistField a) => BinOp a a a
  OMult :: (Num a, PersistField a) => BinOp a a a


-- | A 'Value' is just a type synonym for expressions whose types
-- are not entities but simple values.
type Value a = Expr (Single a)
