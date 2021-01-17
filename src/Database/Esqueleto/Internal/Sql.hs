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
--
-- This module is deprecated as of 3.4.0.1, and will be removed in 3.5.0.0.
module Database.Esqueleto.Internal.Sql
    {-# DEPRECATED "Use Database.Esqueleto.Internal.Internal instead. This module will be removed in 3.5.0.0 " #-}
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
    , valkey
    , valJ
    , deleteKey
    , associateJoin
    ) where

import           Database.Esqueleto.Internal.Internal
