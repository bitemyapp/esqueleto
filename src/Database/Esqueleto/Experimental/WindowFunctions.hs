{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.WindowFunctions
    where

import Control.Arrow (first)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Semigroup (First(..))
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Experimental.Aggregates
import Database.Esqueleto.Internal.Internal
       ( IdentInfo
       , NeedParens(..)
       , OrderBy
       , SideData(..)
       , SomeValue(..)
       , SqlExpr(..)
       , SqlQuery(..)
       , SqlSelect(..)
       , ToSomeValues(..)
       , Value(..)
       , asc
       , noMeta
       , parens
       , parensM
       , select
       , uncommas'
       , unsafeSqlFunction
       , val
       , (?.)
       , (^.)
       )
import Database.Esqueleto.Internal.PersistentImport
       ( Entity
       , EntityField
       , PersistEntity
       , PersistField
       , PersistValue(..)
       , SqlReadT
       , fromPersistValue
       )

--( "LAG(?) OVER (PARTITION BY ?, ? ORDER BY ? ASC ROWS BETWEEN ? PRECEEDING AND UNBOUNDED FOLLOWING)"
--, [PersistInt64 10,PersistInt64 10,PersistBool True,PersistInt64 10,PersistInt64 1]
--)
example =
    lag_ (val @Int64 10) Nothing Nothing `over_`
        (  partitionBy_ (val @Int64 10, val True)
        <> frame_ (rows $ between (preceeding 1) unboundedFollowing)
        <> orderBy_ [asc (val @Int64 10)]
        )

data NeedsWindow a

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

-- Phantom helper type
data PartitionBy
data Window = Window
    { windowPartitionBy :: Maybe (First (SqlExpr PartitionBy))
    , windowOrderBy     :: Maybe [SqlExpr OrderBy]
    , windowFrame       :: Maybe (First Frame)
    }

partitionBy_ :: ToSomeValues a => a -> Window
partitionBy_ expr = mempty{windowPartitionBy = Just $ First $ ERaw noMeta $ \_ info ->
    let (b, v) = uncommas' $ fmap (\(SomeValue (ERaw _ f)) -> f Never info) $ toSomeValues expr
    in ("PARTITION BY " <> b, v)
                          }

orderBy_ :: [SqlExpr OrderBy] -> Window
orderBy_ []    = mempty
orderBy_ exprs = mempty{windowOrderBy=Just exprs}

class RenderWindow a where
    renderWindow :: IdentInfo -> a -> (TLB.Builder, [PersistValue])
instance RenderWindow () where
    renderWindow _ = mempty
instance RenderWindow Window where
    renderWindow info window =
        let (partition, partitionVal) = maybe mempty ((\(ERaw _ f) -> f Never info) . getFirst) (windowPartitionBy window)
            (order, orderVal) = maybe mempty (first ((<>) " ORDER BY ") . uncommas' . fmap (\(ERaw _ f) -> f Never info)) (windowOrderBy window)
            (frame, frameVal) = maybe mempty (renderWindow info . getFirst) (windowFrame window)
        in (partition <> order <> frame, partitionVal <> orderVal <> frameVal)

instance Semigroup Window where
    (Window a b c) <> (Window a' b' c') = Window (a <> a') (b <> b') (c <> c')

instance Monoid Window where
    mempty = Window mempty mempty mempty
    mappend = (<>)

data Frame = Frame (Maybe FrameKind) FrameBody (Maybe FrameExclusion)

frame_ :: ToFrame frame => frame -> Window
frame_ f = mempty{windowFrame = Just $ First $ toFrame f}

instance RenderWindow Frame where
    renderWindow info (Frame mKind frameBody mExclusion) =
        let (kind, kindVals) = maybe mempty (renderWindow info) mKind
            (exclusion, exclusionVals) = maybe mempty (renderWindow info) mExclusion
            (body, bodyVals) = renderWindow info frameBody
        in (" " <> kind <> body <> exclusion, kindVals <> bodyVals <> exclusionVals)

class ToFrame a where
    toFrame :: a -> Frame
instance ToFrame Frame where
    toFrame = id

newtype FrameKind = FrameKind { unFrameKind :: (TLB.Builder, [PersistValue]) }

instance RenderWindow FrameKind where
    renderWindow _ = unFrameKind

frameKind :: ToFrame frame => TLB.Builder -> frame -> Frame
frameKind tlb frame =
    let Frame _ b e = toFrame frame
    in Frame (Just (FrameKind (tlb <> " ", []))) b e

range :: ToFrame frame => frame -> Frame
range = frameKind "RANGE"

rows :: ToFrame frame => frame -> Frame
rows = frameKind "ROWS"

groups :: ToFrame frame => frame -> Frame
groups = frameKind "GROUPS"

newtype FrameExclusion = FrameExclusion { unFrameExclusion :: (TLB.Builder, [PersistValue]) }

instance RenderWindow FrameExclusion where
    renderWindow _ = unFrameExclusion

frameExclusion :: ToFrame frame => TLB.Builder -> frame -> Frame
frameExclusion tlb frame =
    let Frame k b _ = toFrame frame
    in Frame k b (Just $ FrameExclusion (" EXCLUDE " <> tlb, []))

excludeCurrentRow :: ToFrame frame => frame -> Frame
excludeCurrentRow = frameExclusion "CURRENT ROW"

excludeGroup :: ToFrame frame => frame -> Frame
excludeGroup = frameExclusion "GROUP"

excludeTies :: ToFrame frame => frame -> Frame
excludeTies = frameExclusion "TIES"

excludeNoOthers :: ToFrame frame => frame -> Frame
excludeNoOthers = frameExclusion "NO OTHERS"

-- In order to prevent runtime errors we do some magic rewriting of queries that wouldn't be valid SQL.
-- In the case of an implicit frame end `following 10` would become BETWEEN 10 FOLLOWING AND CURRENT ROW
-- This is illegal so `following 10` instead becomes `BETWEEN CURRENT_ROW AND 10 FOLLOWING`
-- Additionally `BETWEEN` requires that the frame start be before the frame end.
-- To prevent this error the frame will be flipped automatically.
-- i.e. `between (following 10) (preceeding 10)` becomes `BETWEEEN 10 PRECEEDING AND 10 FOLLOWING`
-- therefore `between (following 10) (preceeding 10) === between (preceeding 10) (following 10)
data FrameBody
    = FrameStart FrameRange
    | FrameBetween FrameRange FrameRange

instance ToFrame FrameBody where
    toFrame b = Frame Nothing b Nothing

instance RenderWindow FrameBody where
    renderWindow info (FrameStart (FrameRangeFollowing b)) = renderWindow info (FrameBetween FrameRangeCurrentRow (FrameRangeFollowing b))
    renderWindow info (FrameStart f) = renderWindow info f
    renderWindow info (FrameBetween startRange endRange)
          | startRange > endRange = renderWindow info (FrameBetween endRange startRange)
    renderWindow  info (FrameBetween r r') =
        let (b, v) = renderWindow info r
            (b', v') = renderWindow info r'
        in ("BETWEEN " <> b <> " AND " <> b', v <> v')

instance ToFrame FrameRange where
    toFrame r = Frame Nothing (FrameStart r) Nothing
instance RenderWindow FrameRange where
    renderWindow _ (FrameRangeCurrentRow) = ("CURRENT ROW", [])
    renderWindow _ (FrameRangePreceeding bounds) = renderBounds bounds <> (" PRECEEDING", [])
    renderWindow _ (FrameRangeFollowing bounds) = renderBounds bounds <> (" FOLLOWING", [])

renderBounds :: FrameRangeBound -> (TLB.Builder, [PersistValue])
renderBounds (FrameRangeUnbounded) = ("UNBOUNDED", [])
renderBounds (FrameRangeBounded i) = ("?", [PersistInt64 i])

data FrameRange
    = FrameRangePreceeding FrameRangeBound
    | FrameRangeCurrentRow
    | FrameRangeFollowing FrameRangeBound
    deriving Eq

instance Ord FrameRange where
    FrameRangePreceeding b1 <= FrameRangePreceeding b2 = b1 <= b2
    FrameRangePreceeding _  <= FrameRangeCurrentRow    = True
    FrameRangePreceeding _  <= FrameRangeFollowing  _  = True
    FrameRangeCurrentRow    <= FrameRangePreceeding _  = False
    FrameRangeCurrentRow    <= FrameRangeCurrentRow    = True
    FrameRangeCurrentRow    <= FrameRangeFollowing  _  = True
    FrameRangeFollowing  _  <= FrameRangePreceeding _  = False
    FrameRangeFollowing  _  <= FrameRangeCurrentRow    = False
    FrameRangeFollowing  b1 <= FrameRangeFollowing  b2 = b1 <= b2

data FrameRangeBound
    = FrameRangeUnbounded
    | FrameRangeBounded Int64
    deriving Eq

instance Ord FrameRangeBound where
    FrameRangeUnbounded <= FrameRangeBounded _ = False
    FrameRangeUnbounded <= FrameRangeUnbounded = True
    FrameRangeBounded _ <= FrameRangeUnbounded = True
    FrameRangeBounded a <= FrameRangeBounded b = a <= b

between :: FrameRange -> FrameRange -> FrameBody
between = FrameBetween

unboundedPreceeding :: FrameRange
unboundedPreceeding = FrameRangePreceeding FrameRangeUnbounded

preceeding :: Int64 -> FrameRange
preceeding offset = FrameRangePreceeding (FrameRangeBounded offset)

following :: Int64 -> FrameRange
following offset = FrameRangeFollowing (FrameRangeBounded offset)

unboundedFollowing :: FrameRange
unboundedFollowing = FrameRangeFollowing FrameRangeUnbounded

currentRow :: FrameRange
currentRow = FrameRangeCurrentRow

class Over expr where
    over_ :: RenderWindow window => expr a -> window -> SqlExpr (WindowedValue a)

data WindowedValue a = WindowedValue { unWindowedValue :: a }
instance PersistField a => SqlSelect (SqlExpr (WindowedValue a)) (WindowedValue a) where
    sqlSelectCols info expr = sqlSelectCols info (coerce expr :: SqlExpr a)
    sqlSelectColCount = const 1
    sqlSelectProcessRow _ [pv] = WindowedValue <$> fromPersistValue pv
    sqlSelectProcessRow _ pvs  = WindowedValue <$> fromPersistValue (PersistList pvs)


newtype WindowExpr a = WindowExpr { unsafeWindowExpr :: SqlExpr a }
instance Over WindowExpr where
    (WindowExpr (ERaw _ f)) `over_` window = ERaw noMeta $ \p info ->
        let (b, v) = f Never info
            (w, vw) = renderWindow info window
        in (parensM p $ b <> " OVER " <> parens w , v <> vw)

deriving via WindowExpr instance Over SqlAggregate
