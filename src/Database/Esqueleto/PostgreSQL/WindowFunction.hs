{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Esqueleto.PostgreSQL.WindowFunction
    ( Window, Frame, PartitionBy
    , WindowExpr, sqlExprToWindowContext
    , SqlWindow
    , over_, rowNumber_
    , frame_, partitionBy_, orderBy_
    , lag_, lagWithDefault_
    , range, rows, groups
    , excludeCurrentRow, excludeGroup, excludeTies, excludeNoOthers
    , between_, unboundedFollowing, unboundedPreceding, preceding, following, currentRow
    )
    where

import Database.Esqueleto.Internal.Internal
       ( AggregateContext
       , NeedParens(..)
       , SqlExpr
       , SqlExpr_(..)
       , Value(..)
       , noMeta
       , parens
       , parensM
       , unsafeSqlValue
       , val
       , veryUnsafeCoerceSqlExpr
       , unsafeSqlFunction
       )
import Database.Esqueleto.PostgreSQL.Window
       ( Frame
       , PartitionBy
       , RenderWindow(..)
       , Window
       , between_
       , currentRow
       , excludeCurrentRow
       , excludeGroup
       , excludeNoOthers
       , excludeTies
       , following
       , frame_
       , groups
       , orderBy_
       , partitionBy_
       , preceding
       , range
       , rows
       , unboundedFollowing
       , unboundedPreceding
       )
import Data.Int
import Data.Maybe (fromMaybe)

-- | A datatype tag indicating that the given 'SqlExpr_' is in a window context.
data WindowContext

-- | Coerce a 'SqlExpr' value into a 'SqlWindow' value.
sqlExprToWindowContext :: SqlExpr a -> SqlWindow a
sqlExprToWindowContext = veryUnsafeCoerceSqlExpr

-- | Helper type indicating that the 'SqlExpr_' is for a 'WindowContext'.
type SqlWindow = SqlExpr_ WindowContext

-- | A 'WindowExpr' is an expression that should only be used when combined with
-- 'over_' to form a 'SqlWindow' expression.
newtype WindowExpr a = WindowExpr { unWindowExpr :: SqlExpr a }

-- | Return the row number for the given item in the partition.
--
-- Example:
--
-- @
-- 'insertMany_'
--     [ Numbers { numbersInt = 1, numbersDouble = 2 }
--     , Numbers 2 4
--     , Numbers 3 5
--     , Numbers 6 7
--     ]
-- select $ do
--     n <- 'from' $ 'table' @Numbers
--     'orderBy' ['asc' $ n ^. NumbersInt]
--     pure ( n ^. NumbersInt
--          , 'rowNumber_' `Window.over_` ()
--          )
-- @
--
-- This would return
--
-- +------------+-----------+
-- | NumbersInt | rowNumber |
-- +============+===========+
-- |    1       |    1      |
-- |    2       |    2      |
-- |    3       |    3      |
-- |    6       |    4      |
-- +------------+-----------+
--
-- If we use a 'partitionBy_', then it'll return the row number for the given
-- partition. With the above dataset, and changing the query slightly to do
-- 'partitionBy_' whether or not the @NumbersInt@ is even, then we'd have this
-- query:
--
-- @
-- let isEven_ n =
--         n %. val 2 == val 0
-- select $ do
--     n <- 'from' $ 'table' @Numbers
--     'orderBy' ['asc' $ n ^. NumbersInt]
--     pure ( n ^. NumbersInt
--          , 'rowNumber_' `Window.over_`
--              'partitionBy_' isEven_ (n ^. NumbersInt)
--          )
-- @
--
-- with these results:
--
-- +------------+-----------+
-- | NumbersInt | rowNumber `over_` partitionBy isEven |
-- +============+===========+
-- |    1       |    1      |
-- |    2       |    1      |
-- |    3       |    2      |
-- |    6       |    2      |
-- +------------+-----------+
--
-- 1 and 2 are the first odd and even number, and so get "row number" of 1. 3 is
-- the second odd number, and has now number 2. 6 is the second even number, and
-- has the row number 2 as well.
rowNumber_ :: WindowExpr (Value Int64)
rowNumber_ = WindowExpr $ unsafeSqlValue "ROW_NUMBER()"

-- | Like 'lag_', but can accept a default value for the first value in
-- a partition.
lagWithDefault_
    :: SqlExpr (Value a)
    -- ^ The 'SqlExpr' to lag behind.
    -> Maybe (SqlExpr (Value Int64))
    -- ^ The offset. If 'Nothing' is provided, this is @1@.
    -> SqlExpr (Value a)
    -- ^ The default value. Used for the first element in a partition.
    -> WindowExpr (Value a)
lagWithDefault_ v moffset defaultVal =
  WindowExpr $ unsafeSqlFunction "LAG" (v, offset', defaultVal)
  where
    offset' = fromMaybe (val 1) moffset

-- | Include a value from a previous row.
--
-- @
-- insertMany_
--     [ Numbers { numbersInt = 1, numbersDouble = 2 }
--     , Numbers 2 4
--     , Numbers 3 5
--     , Numbers 6 7
--     ]
-- 'select' $ do
--     n <- 'from' $ 'table' @Numbers
--     orderBy ['asc' $ n ^. NumbersInt]
--     pure ( n ^. NumbersInt
--          , 'lag_' (n ^. NumbersInt) Nothing
--              `over_` ()
--          )
-- @
--
-- We're ordering by the @NumbersInt@. This is the result we get:
--
-- +------------+-----------+
-- | NumbersInt | lag_ NumbersInt |
-- +============+===========+
-- |    1       |    NULL   |
-- |    2       |    1      |
-- |    3       |    2      |
-- |    6       |    3      |
-- +------------+-----------+
--
-- The first row returns @NULL@, since there's nothing before. The second row
-- returns the value from the first row, and the third returns the value from
-- the second row.
lag_
    :: SqlExpr (Value a)
    -- ^ The value to show for a previous row.
    -> Maybe (SqlExpr (Value Int64))
    -- ^ How far back to look. If 'Nothing' is provided, it defaults to 1.
    -> WindowExpr (Value (Maybe a))
lag_ v mOffset =
  WindowExpr $ unsafeSqlFunction "LAG" (v, offset')
  where
    offset' = fromMaybe (val 1) mOffset

class WindowExprC expr where
    over_ :: RenderWindow window => expr a -> window -> SqlExpr_ WindowContext a

instance WindowExprC WindowExpr where
    over_ windowExpr window = overImpl (unWindowExpr windowExpr) window

instance (ctx ~ AggregateContext) => WindowExprC (SqlExpr_ ctx) where
    over_ = overImpl

overImpl :: RenderWindow window => SqlExpr_ ctx a -> window -> SqlExpr_ WindowContext a
overImpl (ERaw _ f) window =
    ERaw noMeta $ \p info ->
        let (b, v) = f Never info
            (w, vw) = renderWindow info window
        in (parensM p $ b <> " OVER " <> parens w , v <> vw)
{--

--( "LAG(?) OVER (PARTITION BY ?, ? ORDER BY ? ASC ROWS BETWEEN ? PRECEEDING AND UNBOUNDED FOLLOWING)"
--, [PersistInt64 10,PersistInt64 10,PersistBool True,PersistInt64 10,PersistInt64 1]
--)

example =
    lag_ (val @Int64 10) Nothing Nothing `over_`
        (  partitionBy_ (val @Int64 10, val True)
        <> frame_ (rows $ between (preceeding 1) unboundedFollowing)
        <> orderBy_ [asc (val @Int64 10)]
        )

example2 = countRows_ @Int64 `over_` ()

lag :: SqlExpr_ ValueContext (Value a) -> WindowExpr a
lag v = lag_ v Nothing Nothing

lag_ :: SqlExpr_ ValueContext a -> Maybe (SqlExpr_ ValueContext Int64) -> Maybe (SqlExpr_ ValueContext a) -> WindowExpr a
lag_ v mOffset mDefaultVal =
    coerce $
    case (mOffset, mDefaultVal) of
      (Just offset, Just defaultVal) ->
          unsafeSqlFunction "LAG" (v, offset, defaultVal)
      (Just offset, Nothing) ->
          unsafeSqlFunction "LAG" (v, offset)
      (Nothing, _) ->
          unsafeSqlFunction "LAG" v
--}
