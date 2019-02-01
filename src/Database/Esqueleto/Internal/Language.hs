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
  , when_
  , then_
  , else_
  ) where

import Database.Esqueleto.Internal.PersistentImport
import Database.Esqueleto.Internal.Internal
