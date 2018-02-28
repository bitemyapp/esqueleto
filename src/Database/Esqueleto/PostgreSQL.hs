{-# LANGUAGE OverloadedStrings
 #-}
-- | This module contain PostgreSQL-specific functions.
--
-- /Since: 2.2.8/
module Database.Esqueleto.PostgreSQL
  ( arrayAggDistinct
  , arrayAgg
  , arrayRemove
  , stringAgg
  , chr
  , now_
  , random_
  ) where

import Database.Esqueleto.Internal.Language hiding (random_)
import Database.Esqueleto.Internal.PersistentImport
import Database.Esqueleto.Internal.Sql
import Data.Time.Clock (UTCTime)

-- | (@array_agg@) Concatenate distinct input values, including @NULL@s, into
-- an array.
--
-- /Since: 2.5.3/
arrayAggDistinct :: SqlExpr (Value a) -> SqlExpr (Value [a])
arrayAggDistinct = arrayAgg . distinct'
  where
    distinct' = unsafeSqlBinOp " " (unsafeSqlValue "DISTINCT")

-- | (@random()@) Split out into database specific modules
-- because MySQL uses `rand()`.
--
-- /Since: 2.6.0/
random_ :: (PersistField a, Num a) => SqlExpr (Value a)
random_ = unsafeSqlValue "RANDOM()"

-- | (@array_agg@) Concatenate input values, including @NULL@s,
-- into an array.
--
-- /Since: 2.2.8/
arrayAgg :: SqlExpr (Value a) -> SqlExpr (Value [a])
arrayAgg = unsafeSqlFunction "array_agg"

-- | (@array_remove@) Remove all elements equal to the given value from the
-- array.
--
-- /Since: 2.5.3/
arrayRemove :: SqlExpr (Value [a]) -> SqlExpr (Value a) -> SqlExpr (Value [a])
arrayRemove arr elem' = unsafeSqlFunction "array_remove" (arr, elem')

-- | (@string_agg@) Concatenate input values separated by a
-- delimiter.
--
-- /Since: 2.2.8/
stringAgg
  :: SqlString s
  => SqlExpr (Value s) -- ^ Input values.
  -> SqlExpr (Value s) -- ^ Delimiter.
  -> SqlExpr (Value s) -- ^ Concatenation.
stringAgg expr delim = unsafeSqlFunction "string_agg" (expr, delim)


-- | (@chr@) Translate the given integer to a character. (Note the result will
-- depend on the character set of your database.)
--
-- /Since: 2.2.11/
chr :: SqlString s => SqlExpr (Value Int) -> SqlExpr (Value s)
chr = unsafeSqlFunction "chr"

now_ :: SqlExpr (Value UTCTime)
now_ = unsafeSqlValue "NOW()"
