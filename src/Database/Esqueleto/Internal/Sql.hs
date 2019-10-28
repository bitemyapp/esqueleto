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
{-# LANGUAGE ConstraintKinds
           , EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , MultiParamTypeClasses
           , OverloadedStrings
           , UndecidableInstances
           , ScopedTypeVariables
           , InstanceSigs
           , Rank2Types
           , CPP
 #-}
-- | This is an internal module, anything exported by this module
-- may change without a major version bump.  Please use only
-- "Database.Esqueleto" if possible.
module Database.Esqueleto.Internal.Sql
  ( -- * The pretty face
    SqlQuery
  , SqlExpr(..)
  , SqlEntity
  , select
  , selectSource
  , delete
  , deleteCount
  , update
  , updateCount
  , insertSelect
  , insertSelectCount
    -- * The guts
  , unsafeSqlCase
  , unsafeSqlBinOp
  , unsafeSqlBinOpComposite
  , unsafeSqlValue
  , unsafeSqlCastAs
  , unsafeSqlFunction
  , unsafeSqlExtractSubField
  , UnsafeSqlFunctionArgument
  , OrderByClause
  , rawSelectSource
  , runSource
  , rawEsqueleto
  , toRawSql
  , Mode(..)
  , NeedParens(..)
  , IdentState
  , renderExpr
  , initialIdentState
  , IdentInfo
  , SqlSelect(..)
  , veryUnsafeCoerceSqlExprValue
  , veryUnsafeCoerceSqlExprValueList
  -- * Helper functions
  , renderQueryToText
  , renderQuerySelect
  , renderQueryUpdate
  , renderQueryDelete
  , renderQueryInsertInto
  , makeOrderByNoNewline
  , uncommas'
  , parens
  , toArgList
  , builderToText
  , Ident(..)
  ) where

import Database.Esqueleto.Internal.Internal
