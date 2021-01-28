{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Database.Esqueleto.Experimental.Aggregates
    where

import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.Writer                   as W
import           Data.Coerce                                  (Coercible,
                                                               coerce)
import           Database.Esqueleto.Internal.Internal         (EntityTy,
                                                               EntityTyToValueTy,
                                                               GroupByClause (..),
                                                               MaybeEntityTy,
                                                               MaybeEntityTyToMaybeValueTy,
                                                               MaybeValueTy,
                                                               MaybeValueTyToMaybeEntityTy,
                                                               SideData (..),
                                                               SqlExpr (..),
                                                               SqlQuery (..),
                                                               SqlSelect (..),
                                                               ToSomeValues (..),
                                                               UnMaybeTy,
                                                               Value (..),
                                                               ValueTy,
                                                               ValueTyToEntityTy,
                                                               noMeta, select,
                                                               unsafeSqlFunction,
                                                               (?.), (^.))
import           Database.Esqueleto.Internal.PersistentImport (Entity,
                                                               EntityField,
                                                               PersistEntity,
                                                               PersistField,
                                                               SqlReadT)

-- Phantom data type that doesn't admit a SqlSelect forcing the use of selectAggregate
data Aggregate a

type instance EntityTy (Aggregate (Entity ent)) ent = Entity ent
type instance ValueTy (Aggregate (Value val)) val = Value val
type instance EntityTyToValueTy (Aggregate (Entity ent)) val = Aggregate (Value val)
type instance ValueTyToEntityTy (Aggregate (Value val)) ent = Aggregate (Entity ent)

type instance MaybeEntityTy (Aggregate (Maybe (Entity ent))) ent = Maybe (Entity ent)
type instance MaybeValueTy (Aggregate (Value (Maybe val))) val = Value (Maybe val)
type instance MaybeEntityTyToMaybeValueTy (Aggregate (Maybe (Entity ent))) val = Aggregate (Value (Maybe val))
type instance MaybeValueTyToMaybeEntityTy (Aggregate (Value (Maybe val))) ent = Aggregate (Maybe (Entity ent))
type instance UnMaybeTy (Aggregate (Value (Maybe val))) = Aggregate (Value val)
type instance UnMaybeTy (Aggregate (Maybe (Entity ent))) = Aggregate (Entity ent)

test ent field y other = do
    groupBy (ent, y) $ \(ent', y') ->
        pure (ent' ?. field, y', sum_ other, countRows_)

class CountRowsFn a where
    countRows_ :: SqlExpr a
    countRows_ = ERaw noMeta $ \_ _ -> ("COUNT(*)", [])
instance Integral n => CountRowsFn (Value n)
instance Integral n => CountRowsFn (Aggregate (Value n))

-- Tuple magic, only SqlExprs are on the leaves.
-- The Coercible instance from the SqlExpr a -> SqlExpr b allows 0 cost casting
class Coercible a r => Aggregateable a r | a -> r, r -> a where
    toAggregate :: a -> r
    toAggregate = coerce
    fromAggregate :: r -> a
    fromAggregate = coerce
instance Aggregateable () () where
instance Aggregateable (SqlExpr (Value a)) (SqlExpr (Aggregate (Value a))) where
instance Aggregateable (SqlExpr (Entity a)) (SqlExpr (Aggregate (Entity a))) where
instance Aggregateable (SqlExpr (Maybe (Entity a))) (SqlExpr (Aggregate (Maybe (Entity a)))) where
instance (Aggregateable a ra, Aggregateable b rb) => Aggregateable (a,b) (ra, rb) where
instance
    ( Aggregateable a ra
    , Aggregateable b rb
    , Aggregateable c rc
    ) => Aggregateable (a,b,c) (ra,rb,rc) where
instance
    ( Aggregateable a ra
    , Aggregateable b rb
    , Aggregateable c rc
    , Aggregateable d rd
    ) => Aggregateable (a,b,c,d) (ra,rb,rc,rd) where
instance
    ( Aggregateable a ra
    , Aggregateable b rb
    , Aggregateable c rc
    , Aggregateable d rd
    , Aggregateable e re
    ) => Aggregateable (a,b,c,d,e) (ra,rb,rc,rd,re) where
instance
    ( Aggregateable a ra
    , Aggregateable b rb
    , Aggregateable c rc
    , Aggregateable d rd
    , Aggregateable e re
    , Aggregateable f rf
    ) => Aggregateable (a,b,c,d,e,f) (ra,rb,rc,rd,re,rf) where
instance
    ( Aggregateable a ra
    , Aggregateable b rb
    , Aggregateable c rc
    , Aggregateable d rd
    , Aggregateable e re
    , Aggregateable f rf
    , Aggregateable g rg
    ) => Aggregateable (a,b,c,d,e,f,g) (ra,rb,rc,rd,re,rf,rg) where
instance
    ( Aggregateable a ra
    , Aggregateable b rb
    , Aggregateable c rc
    , Aggregateable d rd
    , Aggregateable e re
    , Aggregateable f rf
    , Aggregateable g rg
    , Aggregateable h rh
    ) => Aggregateable (a,b,c,d,e,f,g,h) (ra,rb,rc,rd,re,rf,rg,rh) where

sum_ :: Integral n => SqlExpr (Value a) -> SqlExpr (Aggregate (Value n))
sum_ = coerce . unsafeSqlFunction "SUM"

groupBy :: (ToSomeValues a, Aggregateable a a', Aggregateable b b') => a -> (a' -> SqlQuery b') -> SqlQuery b
groupBy a f = do
    Q $ W.tell $ mempty{sdGroupByClause = GroupBy $ toSomeValues a }
    fmap fromAggregate $ f $ toAggregate a
