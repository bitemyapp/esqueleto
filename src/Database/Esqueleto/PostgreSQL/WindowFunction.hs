{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Esqueleto.PostgreSQL.WindowFunction
    ( Window, Frame, PartitionBy
    , WindowExpr, liftExpr
    , SqlWindow
    , over_, rowNumber_
    , frame_, partitionBy_, orderBy_
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
       , veryUnsafeCoerceSqlExpr
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

data WindowContext
liftExpr :: SqlExpr a -> SqlExpr_ WindowContext a
liftExpr = veryUnsafeCoerceSqlExpr

-- | Helper type indicating that the 'SqlExpr_' is for a 'WindowContext'.
type SqlWindow = SqlExpr_ WindowContext

newtype WindowExpr a = WindowExpr { unWindowExpr :: SqlExpr a }

rowNumber_ :: WindowExpr (Value Integer)
rowNumber_ = WindowExpr $ unsafeSqlValue "ROW_NUMBER()"

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
