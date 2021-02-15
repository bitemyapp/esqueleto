{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.Aggregates
    where

import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Writer as W
import Data.Coerce (Coercible, coerce)
import Data.Proxy
import Database.Esqueleto.Internal.Internal
       ( GroupByClause(..)
       , SideData(..)
       , SqlExpr(..)
       , SqlQuery(..)
       , SqlQueryHaving(..)
       , SqlSelect(..)
       , ToSomeValues(..)
       , noMeta
       , select
       , unsafeSqlFunction
       )
import qualified Database.Esqueleto.Internal.Internal as I
import Database.Esqueleto.Internal.PersistentImport
       ( Entity
       , EntityField
       , Key
       , PersistEntity
       , PersistField
       , SqlReadT
       , persistIdField
       )

class SqlExprEntity expr where
    (^.) :: (PersistEntity val, PersistField typ)
        => expr (Entity val)
        -> EntityField val typ
        -> expr typ
    (?.) :: (PersistEntity val, PersistField typ)
        => expr (Maybe (Entity val))
        -> EntityField val typ
        -> expr (Maybe typ)

    -- | Project a field of an entity.
instance SqlExprEntity SqlExpr where
    (^.) = (I.^.)
    (?.) = (I.?.)

newtype SqlAggregate a = SqlAggregate { unsafeSqlAggregate :: SqlExpr a }
deriving via SqlExpr instance SqlExprEntity SqlAggregate
instance forall a. PersistField a => SqlSelect (SqlAggregate a) a where
    sqlSelectCols info (SqlAggregate e) = sqlSelectCols info e
    sqlSelectColCount = const 1
    sqlSelectProcessRow _ = sqlSelectProcessRow (Proxy :: Proxy (SqlExpr a))
instance SqlQueryHaving (SqlAggregate Bool) where
    having expr = Q $ W.tell mempty { sdHavingClause = I.Where (coerce expr) }
instance SqlQueryHaving (SqlAggregate (Maybe Bool)) where
    having expr = Q $ W.tell mempty { sdHavingClause = I.Where (coerce expr) }

test :: (PersistEntity ent, PersistField a, PersistField b, PersistField c)
     => SqlExpr (Maybe (Entity ent))
     -> EntityField ent a
     -> SqlExpr b
     -> SqlExpr c
     -> SqlQuery (SqlExpr (Maybe a), SqlExpr b, SqlExpr (Maybe Int), SqlExpr Int)
test ent field y other = do
    groupBy (ent, y) $ \(ent', y') ->
        pure (ent' ?. field, y', sum_ other, countRows_)

countRows_ :: (PersistField n, Integral n) => SqlAggregate n
countRows_ = SqlAggregate $ ERaw noMeta $ \_ _ -> ("COUNT(*)", [])

-- Tuple magic, only SqlExprs are on the leaves.
-- The Coercible instance from the SqlExpr a -> SqlExpr b allows 0 cost casting
class Coercible a r => Aggregateable a r | a -> r, r -> a where
    toAggregate :: a -> r
    toAggregate = coerce

    fromAggregate :: r -> a
    fromAggregate = coerce

instance Aggregateable () () where
instance Aggregateable (SqlExpr a) (SqlAggregate a) where
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

sum_ :: (PersistField a, PersistField n, Integral n) => SqlExpr a -> SqlAggregate (Maybe n)
sum_ = coerce . unsafeSqlFunction "SUM"

groupBy :: ( ToSomeValues a
           , Aggregateable a a'
           , Aggregateable b b'
           ) => a -> (a' -> SqlQuery b') -> SqlQuery b
groupBy a f = do
    Q $ W.tell $ mempty{sdGroupByClause = GroupBy $ toSomeValues a }
    fmap fromAggregate $ f $ toAggregate a
