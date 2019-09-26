{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
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
  , upsertBy
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
  ) where

import           Database.Esqueleto.Internal.Internal
