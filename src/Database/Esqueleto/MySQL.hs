{-# LANGUAGE OverloadedStrings #-}
-- | This module contain MySQL-specific functions.
--
-- /Since: 2.2.8/
module Database.Esqueleto.MySQL
  ( random_
  ) where

import Database.Esqueleto.Internal.Language hiding (random_)
import Database.Esqueleto.Internal.PersistentImport
import Database.Esqueleto.Internal.Sql

-- | (@random()@) Split out into database specific modules
-- because MySQL uses `rand()`.
--
-- /Since: 2.6.0/
random_ :: (PersistField a, Num a) => SqlExpr (Value a)
random_ = unsafeSqlValue "RAND()"
