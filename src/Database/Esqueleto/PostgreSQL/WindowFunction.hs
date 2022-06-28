{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Esqueleto.PostgreSQL.WindowFunction
    ( Window, Frame, PartitionBy
    , WindowExpr, SqlAggregate
    , over_
    , frame_, partitionBy_, orderBy_
    , range, rows, groups
    , excludeCurrentRow, excludeGroup, excludeTies, excludeNoOthers
    , between, unboundedFollowing, unboundedPreceeding, preceeding, following, currentRow
    )
    where

import           Database.Esqueleto.Internal.Internal (NeedParens (..),
                                                       SqlExpr (..), noMeta,
                                                       parens, parensM)
import           Database.Esqueleto.PostgreSQL.Window (Frame, PartitionBy,
                                                       RenderWindow (..),
                                                       Window, between,
                                                       currentRow,
                                                       excludeCurrentRow,
                                                       excludeGroup,
                                                       excludeNoOthers,
                                                       excludeTies, following,
                                                       frame_, groups, orderBy_,
                                                       partitionBy_, preceeding,
                                                       range, rows,
                                                       unboundedFollowing,
                                                       unboundedPreceeding)

newtype SqlAggregate s a = SqlAggregate { _unSqlAggregate :: SqlExpr a }
newtype WindowExpr a = WindowExpr { _unWindowExpr :: SqlExpr a }

data WindowAggregate
class Over expr where
    over_ :: RenderWindow window => expr a -> window -> SqlAggregate WindowAggregate a

instance Over WindowExpr where
    (WindowExpr (ERaw _ f)) `over_` window = SqlAggregate $ ERaw noMeta $ \p info ->
        let (b, v) = f Never info
            (w, vw) = renderWindow info window
        in (parensM p $ b <> " OVER " <> parens w , v <> vw)

-- Only universally quantified SqlAggregate's can be used
-- TODO Add nicer type error
data NoWindow
deriving via WindowExpr instance (s ~ NoWindow) => Over (SqlAggregate s)

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

newtype WindowExpr a = WindowExpr { unsafeWindowExpr :: SqlExpr a }

lag :: SqlExpr (Value a) -> WindowExpr a
lag v = lag_ v Nothing Nothing

lag_ :: SqlExpr a -> Maybe (SqlExpr Int64) -> Maybe (SqlExpr a) -> WindowExpr a
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
