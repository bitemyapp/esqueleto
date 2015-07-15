{-# LANGUAGE OverloadedStrings
 #-}
-- | This module contain PostgreSQL-specific functions.
--
-- /Since: 2.2.8/
module Database.Esqueleto.PostgreSQL
  ( arrayAgg
  ) where

import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql


-- | (@array_agg@) Concatenate input values, including @NULL@s,
-- into an array.
--
-- /Since: 2.2.8/
arrayAgg :: SqlExpr (Value a) -> SqlExpr (Value [a])
arrayAgg = unsafeSqlFunction "array_agg"
