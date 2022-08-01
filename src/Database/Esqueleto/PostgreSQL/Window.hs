{-# LANGUAGE OverloadedStrings #-}

module Database.Esqueleto.PostgreSQL.Window
    ( Window, Frame, PartitionBy
    , RenderWindow(..)
    , frame_, partitionBy_, orderBy_
    , range, rows, groups
    , excludeCurrentRow, excludeGroup, excludeTies, excludeNoOthers
    , between, unboundedFollowing, unboundedPreceding, preceding, following, currentRow
    )
    where

import Data.Bifunctor (first)
import Data.Semigroup (First(..))
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Internal.Internal
       ( IdentInfo
       , NeedParens(..)
       , OrderBy
       , SomeValue(..)
       , SqlExpr_(..)
       , ToSomeValues(..)
       , ValueContext
       , noMeta
       , uncommas'
       )
import Database.Esqueleto.Internal.PersistentImport (PersistValue)
import Database.Esqueleto.PostgreSQL.Window.Frame
       ( Frame
       , ToFrame(..)
       , between
       , currentRow
       , excludeCurrentRow
       , excludeGroup
       , excludeNoOthers
       , excludeTies
       , following
       , groups
       , preceding
       , range
       , renderFrame
       , rows
       , unboundedFollowing
       , unboundedPreceding
       )

-- | A monoidal representation of a Window to be used with a Window Function
--
-- A window is defined using the helper functions 'partitionBy_', 'orderBy_' and 'frame_'
data Window = Window
    { windowPartitionBy :: Maybe (First (SqlExpr_ ValueContext PartitionBy))
    , windowOrderBy     :: Maybe [SqlExpr_ ValueContext OrderBy]
    , windowFrame       :: Maybe (First Frame)
    }

instance Semigroup Window where
    (Window a b c) <> (Window a' b' c') = Window (a <> a') (b <> b') (c <> c')

instance Monoid Window where
    mempty = Window mempty mempty mempty
    mappend = (<>)

-- Phantom helper type
data PartitionBy

-- | PARTITION BY
--
-- Used to divide the result set into partitions for the window function to operate over
partitionBy_ :: ToSomeValues a => a -> Window
partitionBy_ expr =
    mempty{ windowPartitionBy = Just $ First $ ERaw noMeta $ const impl }

  where
      impl info =
            let (b, v) = renderSomeValues info (toSomeValues expr)
            in ("PARTITION BY " <> b, v)

      renderSomeValues info someValues =
          uncommas' $ fmap (\(SomeValue (ERaw _ f)) -> f Never info) someValues

-- | ORDER BY
--
-- Order the values in the given partition
orderBy_ :: [SqlExpr_ ValueContext OrderBy] -> Window
orderBy_ []    = mempty
orderBy_ exprs = mempty{ windowOrderBy = Just exprs }

-- | FRAME
--
-- Defines a set of rows relative to the current row to include in the window
--
-- e.g.
-- frame_ (between (preceding 10) (following 10))
frame_ :: ToFrame frame => frame -> Window
frame_ f = mempty{windowFrame = Just $ First $ toFrame f}

class RenderWindow a where
    renderWindow :: IdentInfo -> a -> (TLB.Builder, [PersistValue])
instance RenderWindow () where
    renderWindow _ = mempty
instance RenderWindow Window where
    renderWindow info window =
        let (partition, partitionVal) = maybe mempty ((\(ERaw _ f) -> f Never info) . getFirst) (windowPartitionBy window)
            (order, orderVal) = maybe mempty (first ((<>) " ORDER BY ") . uncommas' . fmap (\(ERaw _ f) -> f Never info)) (windowOrderBy window)
            (frame, frameVal) = maybe mempty (renderFrame info . getFirst) (windowFrame window)
        in (partition <> order <> frame, partitionVal <> orderVal <> frameVal)
