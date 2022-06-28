{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Database.Esqueleto.PostgreSQL.WindowFunction
    ( Window, Frame, PartitionBy
    , WindowExpr
    , over_, sum_, rowNumber_
    , frame_, partitionBy_, orderBy_
    , range, rows, groups
    , excludeCurrentRow, excludeGroup, excludeTies, excludeNoOthers
    , between, unboundedFollowing, unboundedPreceding, preceding, following, currentRow
    )
    where

import qualified Data.Text.Lazy.Builder                       as TLB
import           Database.Esqueleto.Internal.Internal         (NeedParens (..),
                                                               SqlExpr (..),
                                                               SqlSelect (..),
                                                               UnsafeSqlFunctionArgument,
                                                               Value (..),
                                                               materializeExpr,
                                                               noMeta, parens,
                                                               parensM,
                                                               unsafeSqlFunction,
                                                               unsafeSqlValue)
import           Database.Esqueleto.Internal.PersistentImport (PersistField (..),
                                                               PersistValue (..))
import           Database.Esqueleto.PostgreSQL.Window         (Frame,
                                                               PartitionBy,
                                                               RenderWindow (..),
                                                               Window, between,
                                                               currentRow,
                                                               excludeCurrentRow,
                                                               excludeGroup,
                                                               excludeNoOthers,
                                                               excludeTies,
                                                               following,
                                                               frame_, groups,
                                                               orderBy_,
                                                               partitionBy_,
                                                               preceding,
                                                               range, rows,
                                                               unboundedFollowing,
                                                               unboundedPreceding)

newtype WindowExpr a = WindowExpr { _unWindowExpr :: SqlExpr a }

unsafeWindowFunction :: UnsafeSqlFunctionArgument a => TLB.Builder -> a -> WindowExpr (Value b)
unsafeWindowFunction functionName arguments =
    WindowExpr $ unsafeSqlFunction functionName arguments

sum_ :: (PersistField a, PersistField b) => SqlExpr (Value a) -> WindowExpr (Value (Maybe b))
sum_ = unsafeWindowFunction "SUM"

rowNumber_ :: WindowExpr (Value Integer)
rowNumber_ = WindowExpr $ unsafeSqlValue "ROW_NUMBER()"

over_ :: RenderWindow window => WindowExpr a -> window -> SqlExpr a
(WindowExpr (ERaw _ f)) `over_` window =
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
