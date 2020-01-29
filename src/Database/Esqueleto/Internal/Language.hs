{-# LANGUAGE DeriveDataTypeable
           , EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
           , GADTs
 #-}
-- | This is an internal module, anything exported by this module
-- may change without a major version bump.  Please use only
-- "Database.Esqueleto" if possible.
module Database.Esqueleto.Internal.Language
  ( -- * The pretty face
    from
  , fromParts
  , Value(..)
  , ValueList(..)
  , SomeValue(..)
  , ToSomeValues(..)
  , InnerJoin(..)
  , CrossJoin(..)
  , LeftOuterJoin(..)
  , RightOuterJoin(..)
  , FullOuterJoin(..)
  , OnClauseWithoutMatchingJoinException(..)
  , OrderBy
  , DistinctOn
  , Update
  , Insertion
  , LockingKind(..)
  , SqlString
  , ToBaseId(..)
    -- * The guts
  , JoinKind(..)
  , IsJoinKind(..)
  , BackendCompatible(..)
  , PreprocessedFrom
  , From
  , FromPreprocess
  , FromParts(..)
  , when_
  , then_
  , else_
  , where_, on, on_, groupBy, orderBy, rand, asc, desc, limit, offset
             , distinct, distinctOn, don, distinctOnOrderBy, having, locking
             , sub_select, (^.), (?.)
             , val, isNothing, just, nothing, joinV, withNonNull
             , countRows, count, countDistinct
             , not_, (==.), (>=.), (>.), (<=.), (<.), (!=.), (&&.), (||.)
             , between, (+.), (-.), (/.), (*.)
             , random_, round_, ceiling_, floor_
             , min_, max_, sum_, avg_, castNum, castNumM
             , coalesce, coalesceDefault
             , lower_, upper_, trim_, ltrim_, rtrim_, length_, left_, right_
             , like, ilike, (%), concat_, (++.), castString
             , subList_select, valList, justList
             , in_, notIn, exists, notExists
             , set, (=.), (+=.), (-=.), (*=.), (/=.)
             , case_, toBaseId, (<#), (<&>)
  , subSelect
  , subSelectMaybe
  , subSelectCount
  , subSelectList
  , subSelectForeign
  , subSelectUnsafe
  ) where

import Database.Esqueleto.Internal.PersistentImport
import Database.Esqueleto.Internal.Internal
