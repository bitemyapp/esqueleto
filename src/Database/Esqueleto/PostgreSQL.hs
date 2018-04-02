{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings
           , GADTs, CPP
 #-}
-- | This module contain PostgreSQL-specific functions.
--
-- /Since: 2.2.8/
module Database.Esqueleto.PostgreSQL
  ( AggMode(..)
  , arrayAggDistinct
  , arrayAgg
  , arrayAggWith
  , arrayRemove
  , arrayRemoveNull
  , stringAgg
  , stringAggWith
  , maybeArray
  , chr
  , now_
  , random_
  -- * Internal
  , unsafeSqlAggregateFunction
  ) where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import qualified Data.Text.Internal.Builder                   as TLB
import           Data.Time.Clock                              (UTCTime)
import           Database.Esqueleto.Internal.Language         hiding (random_)
import           Database.Esqueleto.Internal.PersistentImport
import           Database.Esqueleto.Internal.Sql

-- | (@random()@) Split out into database specific modules
-- because MySQL uses `rand()`.
--
-- /Since: 2.6.0/
random_ :: (PersistField a, Num a) => SqlExpr (Value a)
random_ = unsafeSqlValue "RANDOM()"

-- | Empty array literal. (@val []@) does unfortunately not work
emptyArray :: SqlExpr (Value [a])
emptyArray = unsafeSqlValue "'{}'"

-- | Coalesce an array with an empty default value
maybeArray ::
     (PersistField a, PersistField [a])
  => SqlExpr (Value (Maybe [a]))
  -> SqlExpr (Value [a])
maybeArray x = coalesceDefault [x] (emptyArray)

-- | Aggregate mode
data AggMode = AggModeAll -- ^ ALL
             | AggModeDistinct -- ^ DISTINCT
  deriving (Show)

-- | (Internal) Create a custom aggregate functions with aggregate mode
--
-- /Do/ /not/ use this function directly, instead define a new function and give
-- it a type (see `unsafeSqlBinOp`)
unsafeSqlAggregateFunction ::
     UnsafeSqlFunctionArgument a
  => TLB.Builder
  -> AggMode
  -> a
  -> [OrderByClause]
  -> SqlExpr (Value b)
unsafeSqlAggregateFunction name mode args orderByClauses =
  ERaw Never $ \info ->
    let (orderTLB, orderVals) = makeOrderByNoNewline info orderByClauses
        -- Don't add a space if we don't have order by clauses
        orderTLBSpace = case orderByClauses of
                          [] -> ""
                          (_:_) -> " "
        (argsTLB, argsVals) =
          uncommas' $ map (\(ERaw _ f) -> f info) $ toArgList args
        aggMode = case mode of
                    AggModeAll -> "" -- ALL is the default, so we don't need to
                                     -- specify it
                    AggModeDistinct -> "DISTINCT "
    in ( name <> parens (aggMode <> argsTLB <> orderTLBSpace <> orderTLB)
       , argsVals <> orderVals
       )

--- | (@array_agg@) Concatenate input values, including @NULL@s,
--- into an array.
arrayAggWith ::
     AggMode
  -> SqlExpr (Value a)
  -> [OrderByClause]
  -> SqlExpr (Value (Maybe [a]))
arrayAggWith = unsafeSqlAggregateFunction "array_agg"

--- | (@array_agg@) Concatenate input values, including @NULL@s,
--- into an array.
arrayAgg :: (PersistField a) => SqlExpr (Value a) -> SqlExpr (Value (Maybe [a]))
arrayAgg x = arrayAggWith AggModeAll x []

-- | (@array_agg@) Concatenate distinct input values, including @NULL@s, into
-- an array.
--
-- /Since: 2.5.3/
arrayAggDistinct ::
     (PersistField a, PersistField [a])
  => SqlExpr (Value a)
  -> SqlExpr (Value (Maybe [a]))
arrayAggDistinct x = arrayAggWith AggModeDistinct x []


-- | (@array_remove@) Remove all elements equal to the given value from the
-- array.
--
-- /Since: 2.5.3/
arrayRemove :: SqlExpr (Value [a]) -> SqlExpr (Value a) -> SqlExpr (Value [a])
arrayRemove arr elem' = unsafeSqlFunction "array_remove" (arr, elem')

-- | Remove @NULL@ values from an array
arrayRemoveNull :: SqlExpr (Value [Maybe a]) -> SqlExpr (Value [a])
-- This can't be a call to arrayRemove because it changes the value type
arrayRemoveNull x = unsafeSqlFunction "array_remove" (x, unsafeSqlValue "NULL")


-- | (@string_agg@) Concatenate input values separated by a
-- delimiter.
stringAggWith ::
     SqlString s
  => AggMode -- ^ Aggregate mode (ALL or DISTINCT)
  -> SqlExpr (Value s) -- ^ Input values.
  -> SqlExpr (Value s) -- ^ Delimiter.
  -> [OrderByClause] -- ^ ORDER BY clauses
  -> SqlExpr (Value (Maybe s)) -- ^ Concatenation.
stringAggWith mode expr delim os =
  unsafeSqlAggregateFunction "string_agg" mode (expr, delim) os

-- | (@string_agg@) Concatenate input values separated by a
-- delimiter.
--
-- /Since: 2.2.8/
stringAgg ::
     SqlString s
  => SqlExpr (Value s) -- ^ Input values.
  -> SqlExpr (Value s) -- ^ Delimiter.
  -> SqlExpr (Value (Maybe s)) -- ^ Concatenation.
stringAgg expr delim = stringAggWith AggModeAll expr delim []

-- | (@chr@) Translate the given integer to a character. (Note the result will
-- depend on the character set of your database.)
--
-- /Since: 2.2.11/
chr :: SqlString s => SqlExpr (Value Int) -> SqlExpr (Value s)
chr = unsafeSqlFunction "chr"

now_ :: SqlExpr (Value UTCTime)
now_ = unsafeSqlValue "NOW()"
