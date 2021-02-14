{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.WindowFunctions
    where

import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Semigroup (First(..))
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Experimental.Aggregates
import Database.Esqueleto.Internal.Internal
       ( NeedParens(..)
       , SideData(..)
       , SqlExpr(..)
       , SqlQuery(..)
       , SqlSelect(..)
       , ToSomeValues(..)
       , Value(..)
       , noMeta
       , select
       , unsafeSqlFunction
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


data Window = Window
    { windowPartitionBy :: Maybe (First (TLB.Builder, [PersistValue]))
    , windowOrderBy     :: Maybe (First (TLB.Builder, [PersistValue]))
    , windowFrame       :: Maybe (First Frame)
    }

class RenderWindow a where
    renderWindow :: a -> (TLB.Builder, [PersistValue])
instance RenderWindow () where
    renderWindow = mempty
instance RenderWindow Window where
    renderWindow window =
        let (p, pVal) = maybe mempty getFirst $ windowPartitionBy window
            (o, oVal) = maybe mempty getFirst $ windowOrderBy window
            (f, fVal) = maybe mempty (renderWindow . getFirst) (windowFrame window)
        in (p <> o <> f, pVal <> oVal <> fVal)

instance Semigroup Window where
    (Window a b c) <> (Window a' b' c') = Window (a <> a') (b <> b') (c <> c')

instance Monoid Window where
    mempty = Window mempty mempty mempty
    mappend = (<>)

data Frame = Frame (Maybe FrameKind) FrameBody (Maybe FrameExclusion)

frame :: ToFrame frame => frame -> Window
frame f = mempty{windowFrame = Just $ First $ toFrame f}

instance RenderWindow Frame where
    renderWindow (Frame mKind frameBody mExclusion) =
        let (kind, kindVals) = maybe mempty renderWindow mKind
            (exclusion, exclusionVals) = maybe mempty renderWindow mExclusion
            (body, bodyVals) = renderWindow frameBody
        in (kind <> body <> exclusion, kindVals <> bodyVals <> exclusionVals)

class ToFrame a where
    toFrame :: a -> Frame
instance ToFrame Frame where
    toFrame = id

newtype FrameKind = FrameKind { unFrameKind :: (TLB.Builder, [PersistValue]) }

instance RenderWindow FrameKind where
    renderWindow = unFrameKind

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
    renderWindow = unFrameExclusion

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

data FrameBody
    = FrameStart FrameRange
    | FrameBetween FrameRange FrameRange

instance ToFrame FrameBody where
    toFrame b = Frame Nothing b Nothing

instance RenderWindow FrameBody where
    renderWindow (FrameStart (FrameRangeFollowing b)) = renderWindow (FrameBetween FrameRangeCurrentRow (FrameRangeFollowing b))
    renderWindow (FrameStart f) = renderWindow f
    renderWindow (FrameBetween startRange endRange)
          | startRange > endRange = renderWindow (FrameBetween endRange startRange)
    renderWindow (FrameBetween r r') =
        let (b, v) = renderWindow r
            (b', v') = renderWindow r'
        in ("BETWEEN " <> b <> " AND " <> b', v <> v')

instance ToFrame FrameRange where
    toFrame r = Frame Nothing (FrameStart r) Nothing
instance RenderWindow FrameRange where
    renderWindow (FrameRangeCurrentRow) = ("CURRENT ROW", [])
    renderWindow (FrameRangePreceeding bounds) = renderBounds bounds <> (" PRECEEDING", [])
    renderWindow (FrameRangeFollowing bounds) = renderBounds bounds <> (" FOLLOWING", [])

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
            (w, vw) = renderWindow window
        in (b <> " OVER (" <> w <> ")", v <> vw)

deriving via WindowExpr instance Over SqlAggregate
