{-# LANGUAGE OverloadedStrings #-}

-- | This module contain SQLite-specific functions.
--
-- @since 2.2.8
module Database.Esqueleto.SQLite
  ( random_
  ) where

import Database.Esqueleto.Internal.Internal hiding (random_)
import Database.Esqueleto.Internal.PersistentImport

-- | (@random()@) Split out into database specific modules
-- because MySQL uses `rand()`.
--
-- /Since: 2.6.0/
random_ :: (PersistField a, Num a) => SqlExpr a
random_ = unsafeSqlValue "RANDOM()"
