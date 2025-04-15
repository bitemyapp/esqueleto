{-# LANGUAGE OverloadedStrings #-}

-- | This module contain MySQL-specific functions.
--
-- @since 2.2.8
module Database.Esqueleto.MySQL
    ( random_
    , lockInShareMode
    ) where

import Database.Esqueleto.Internal.Internal hiding (random_)
import Database.Esqueleto.Internal.PersistentImport

-- | (@random()@) Split out into database specific modules
-- because MySQL uses `rand()`.
--
-- @since 2.6.0
random_ :: (PersistField a, Num a) => SqlExpr (Value a)
random_ = unsafeSqlValue "RAND()"

-- | @LOCK IN SHARE MODE@ syntax.
--
-- Example:
--
-- @
--  'locking' 'lockInShareMode'
-- @
--
-- @since 3.6.0.0
lockInShareMode :: LockingKind
lockInShareMode = LockInShareMode
