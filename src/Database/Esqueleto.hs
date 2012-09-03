{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs #-}

module Database.Esqueleto
  ( -- * Esqueleto's Language
    Esqueleto(..)
  , from

    -- * SQL backend
  , SqlQuery
  , SqlExpr
  , select

    -- * Re-exports
  , module Database.Persist.Store
  , module Database.Persist.GenericSql
  ) where

import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
import Database.Persist.Store
import Database.Persist.GenericSql

-- test :: (PersistField t, PersistEntity a, PersistEntity b, PersistEntityBackend a ~ SqlPersist, PersistEntityBackend b ~ SqlPersist) => EntityField b t -> SqlPersist IO [(Entity a, Single t, Entity b)]
test f = select $ do
           (x,y,z) <- from
           where_ (z^.f ==. y^.f)
           return (x, y^.f, z)
