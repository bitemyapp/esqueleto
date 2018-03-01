{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings
 #-}
-- | This module contain PostgreSQL-specific functions.
--
-- /Since: 2.2.8/
module Database.Esqueleto.PostgreSQL
  ( arrayAggDistinct
  , arrayAggDistinct'
  , arrayAgg
  , arrayAgg'
  , arrayRemove
  , arrayRemoveNull
  , array
  , stringAgg
  , chr
  , now_
  , random_
  , toChar
  ) where

import           Data.Text                                    (Text)
import           Data.Time.Clock                              (UTCTime)
import           Database.Esqueleto.Internal.Language         hiding (random_)
import           Database.Esqueleto.Internal.PersistentImport
import           Database.Esqueleto.Internal.Sql


-- | (@array_agg@) Concatenate distinct input values, including @NULL@s, into
-- an array.
--
-- /Since: 2.5.3/
arrayAggDistinct :: SqlExpr (Value a) -> SqlExpr (Value [a])
arrayAggDistinct = arrayAgg . distinct'
  where
    distinct' = unsafeSqlBinOp " " (unsafeSqlValue "DISTINCT")

-- | Like arrayAggDistinct but removes @NULL@ values
arrayAggDistinct' :: SqlExpr (Value (Maybe a)) -> SqlExpr (Value [a])
arrayAggDistinct' = arrayRemoveNull . arrayAggDistinct

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

-- | Like 'array_agg' but removes @NULL@ values
arrayAgg' :: PersistField [a] => SqlExpr (Value (Maybe a)) -> SqlExpr (Value [a])
arrayAgg' =  arrayRemoveNull . arrayAgg

-- | Create a singleton array (postgres)
array :: SqlExpr (Value a) -> SqlExpr (Value [a])
array = unsafeSqlFunction "array"

-- | (@array_remove@) Remove all elements equal to the given value from the
-- array.
--
-- /Since: 2.5.3/
arrayRemove :: SqlExpr (Value [a]) -> SqlExpr (Value a) -> SqlExpr (Value [a])
arrayRemove arr elem' = unsafeSqlFunction "array_remove" (arr, elem')

-- | Remove @NULL@ values from an array
arrayRemoveNull :: SqlExpr (Value [Maybe a]) -> SqlExpr (Value [a])
arrayRemoveNull x = unsafeSqlFunction "array_remove" (x, unsafeSqlValue "NULL")


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

-- | Format a value with a format string
toChar :: SqlExpr (Value (Maybe UTCTime))
              -> SqlExpr (Value Text)
              -> SqlExpr (Value (Maybe Text))
toChar time formatstring = unsafeSqlFunction "to_char" (time, formatstring)
