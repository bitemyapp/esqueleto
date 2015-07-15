{-# LANGUAGE OverloadedStrings
 #-}
-- | This module contain PostgreSQL-specific functions.
--
-- /Since: 2.2.8/
module Database.Esqueleto.PostgreSQL
  ( arrayAgg
  , stringAgg
  ) where

import Data.String (IsString)

import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql


-- | (@array_agg@) Concatenate input values, including @NULL@s,
-- into an array.
--
-- /Since: 2.2.8/
arrayAgg :: SqlExpr (Value a) -> SqlExpr (Value [a])
arrayAgg = unsafeSqlFunction "array_agg"


-- | (@string_agg@) Concatenate input values separated by a
-- delimiter.
--
-- /Since: 2.2.8/
stringAgg
  :: IsString s
  => SqlExpr (Value s) -- ^ Input values.
  -> SqlExpr (Value s) -- ^ Delimiter.
  -> SqlExpr (Value s) -- ^ Concatenation.
stringAgg expr delim = unsafeSqlFunction "string_agg" (expr, delim)
