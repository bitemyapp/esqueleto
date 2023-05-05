{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

= Design notes

OK, so this is tricky. We have two operators with the same name but two very
different types.

@
(==.) :: SqlExpr (Value a)   -> SqlExpr (Value a) -> SqlExpr (Value Bool)
(==.) :: EntityField rec typ -> typ               -> Filter rec
@

Usage:

@
where_ $ foo ^. FooName ==. val 10

selectList [FooName ==. 10]
@

The really tricky thing here is that plain @typ@. That's going to wreak all
kinds of hacov-y hell on attempting to have good type inference.

So, first attempt: a type class with three parameters:

@
class SqlEquality a b c where
    (==.) :: a -> b -> c

instance SqlEquality
@



 -}

-- | This module provides exports that are compatible with @persistent@ and
-- @esqueleto@, so you don't have to worry about disambiguating quite so much.
module Database.Esqueleto.Compat where

import Data.Kind
import Database.Esqueleto.Experimental (SqlExpr, Value)
import qualified Database.Esqueleto.Experimental as Esqueleto
import Database.Persist.Sql (EntityField, Filter, PersistField)
import qualified Database.Persist.Sql as Persist
import GHC.TypeLits

-- | A class for comparing for equality in @persistent@ and @esqueleto@. The
-- first two type parameters are the inputs to the binary operator, and the
-- final one is the result type.
class (EqTypeCompatible a b c) => SqlEquality a b c where
    (==.) :: a -> b -> c
    (!=.) :: a -> b -> c

class EqTypeCompatible a b c

instance (NotSqlExpr t r) => EqTypeCompatible (EntityField e t) t r
instance EqTypeCompatible (SqlExpr (Value a)) (SqlExpr (Value b)) (SqlExpr (Value Bool))

type family NotSqlExpr t r where
    NotSqlExpr (SqlExpr _) (Filter _) =
        TypeError
            ( 'Text "It looks like you tried to use the ==. or /=. operator in an ill-typed way."
            ':$$: 'Text "For a Persistent expression, you don't need 'val'."
            ':$$: 'Text "For an esqueleto expression, you need to project the field out."
            )
    NotSqlExpr t r = () :: Constraint

instance
    (TypeError ('Text "no")) =>
        EqTypeCompatible (EntityField e t) (SqlExpr b) c

-- | An alias for '!=.' which follows the Haskell convention of using '/=' as
-- the inequality operator.
(/=.) :: SqlEquality a b c => a -> b -> c
(/=.) = (!=.)

-- | The basic instance for @persistent@'s 'Persist.==.' operator.
instance
    (ent ~ ent', typ ~ typ', PersistField typ, NotSqlExpr typ' (Filter ent')
    , field ~ EntityField ent typ)
  =>
    SqlEquality field typ' (Filter ent')
  where
    (==.) = (Persist.==.)
    (!=.) = (Persist.!=.)

-- | The basic instance for @esqueleto@'s 'Esqueleto.==.' operator.
instance
    (a ~ b, PersistField a)
  =>
    SqlEquality (SqlExpr (Value a)) (SqlExpr (Value b)) (SqlExpr (Value Bool))
  where
    (==.) = (Esqueleto.==.)
    (!=.) = (Esqueleto.!=.)

