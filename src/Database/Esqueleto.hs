{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs #-}
-- | Main module of @esqueleto@.  This module replaces
-- @Database.Persist@, so instead of importing that module you
-- should just import this one:
--
-- @
-- import Database.Esqueleto
-- @
--
-- If you still have legacy code using @Database.Persist.Query@
-- (which is exported by @Database.Persist@), you may import it
-- qualified:
--
-- @
-- import qualified Database.Persist.Query as OldQuery
-- @
module Database.Esqueleto
  ( -- * @esqueleto@'s Language
    Esqueleto( where_, on, orderBy, asc, desc
             , sub_select, sub_selectDistinct, (^.), (?.)
             , val, isNothing, just, nothing, countRows, not_
             , (==.), (>=.), (>.), (<=.), (<.), (!=.), (&&.), (||.)
             , (+.), (-.), (/.), (*.)
             , set, (=.), (+=.), (-=.), (*=.), (/=.) )
  , from
  , OrderBy
    -- ** Joins
  , InnerJoin(..)
  , CrossJoin(..)
  , LeftOuterJoin(..)
  , RightOuterJoin(..)
  , FullOuterJoin(..)
  , OnClauseWithoutMatchingJoinException(..)

    -- * SQL backend
  , SqlQuery
  , SqlExpr
  , select
  , selectDistinct
  , selectSource
  , selectDistinctSource
  , delete
  , update

    -- * Re-exports
    -- $reexports
  , deleteKey
  , module Database.Persist.GenericSql
  , module Database.Persist.Store
  ) where

import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
import Database.Persist.Store hiding (delete)
import Database.Persist.GenericSql
import qualified Database.Persist.Store


-- | Synonym for 'Database.Persist.Store.delete' that does not
-- clash with @esqueleto@'s 'delete'.
deleteKey :: (PersistStore backend m, PersistEntity val)
          => Key backend val -> backend m ()
deleteKey = Database.Persist.Store.delete

-- $reexports
--
-- We re-export @Database.Persist.Store@ for convenience, since
-- @esqueleto@ currently does not provide a way of doing
-- @insert@s or @update@s.
