{-# LANGUAGE OverloadedStrings #-}

module Database.Esqueleto.PostgreSQL.Window
    ( Window, Frame, PartitionBy
    , RenderWindow(..)
    , frame_, partitionBy_, orderBy_
    , range, rows, groups
    , excludeCurrentRow, excludeGroup, excludeTies, excludeNoOthers
    , between, unboundedFollowing, unboundedPreceeding, preceeding, following, currentRow
    )
    where

import Data.Bifunctor (first)
import Data.Semigroup (First(..))
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Internal.PersistentImport ( PersistValue )
import Database.Esqueleto.Internal.Internal
       ( IdentInfo
       , SqlExpr(..)
       , OrderBy
       , ToSomeValues(..)
       , SomeValue(..)
       , NeedParens(..)
       , uncommas'
       , noMeta
       )
import Database.Esqueleto.PostgreSQL.Window.Frame
    ( Frame
    , ToFrame(..)
    , renderFrame
    , range, rows, groups
    , excludeCurrentRow, excludeGroup, excludeTies, excludeNoOthers
    , between, unboundedFollowing, unboundedPreceeding, preceeding, following, currentRow
    )


data Window = Window
    { windowPartitionBy :: Maybe (First (SqlExpr PartitionBy))
    , windowOrderBy     :: Maybe [SqlExpr OrderBy]
    , windowFrame       :: Maybe (First Frame)
    }

-- Phantom helper type
data PartitionBy

partitionBy_ :: ToSomeValues a => a -> Window
partitionBy_ expr = 
    mempty{ windowPartitionBy = Just $ First $ ERaw noMeta $ const impl }

  where 
      impl info =
            let (b, v) = renderSomeValues info (toSomeValues expr)
            in ("PARTITION BY " <> b, v)

      renderSomeValues info someValues = 
          uncommas' $ fmap (\(SomeValue (ERaw _ f)) -> f Never info) someValues

orderBy_ :: [SqlExpr OrderBy] -> Window
orderBy_ []    = mempty
orderBy_ exprs = mempty{ windowOrderBy = Just exprs }

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

instance Semigroup Window where
    (Window a b c) <> (Window a' b' c') = Window (a <> a') (b <> b') (c <> c')

instance Monoid Window where
    mempty = Window mempty mempty mempty
    mappend = (<>)


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
