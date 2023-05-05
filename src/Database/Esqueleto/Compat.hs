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

instance
    (PersistField typ)
  =>
    SqlEquality (EntityField rec typ) typ (Filter rec)
  where
    (==.) = (Persist.==.)

instance
    (PersistField a)
  =>
    SqlEquality (SqlExpr (Value a)) (SqlExpr (Value a)) (SqlExpr (Value Bool))
  where
    (==.) = (Esqueleto.==.)
@

This is the "most basic" implementation, with the fewest Tricks. However, this
has absolutely awful type inference properties. Consider this model:

@
Foo
    name    Int
@

You'd expect @e ^. FooName ==. val 10@ to type check. Additionally, the
expression @FooName ==. 10@ should also work fine. However, we get ambiguous type variable errors.

@
No instance for (SqlEquality (EntityField Foo Int) b0 (Filter Foo)

No instance for (SqlEquality (SqlExpr (Value Int)) (SqlExpr (Value typ0)) (SqlExpr (Value Bool)))
@

This is a somewhat classic problem with polymorphic classes and values. The
primary difficulty is that we have a polymorphic numeral, which GHC isn't
wanting to resolve. This is a common enough case that we need to support it in
our design.

= Functional Dependencies

Naturally, you want to add some fundeps to aid in type inference. Unfortunately, this doesn't help.

@
class SqlEquality a b c | a b -> c where
    (==.) :: a -> b -> c
@

With this formulation, we still get errors in both simple forms @e ^. FooName ==. val 10@ and @FooName ==. 10@.

Now, it's not *too* often that we sit around with a bare expression like this. Maybe a type annotation on top of the simpler form will help?

@
lol' :: Filter Foo
lol' = FooName ==. 10
@

Unfortunately, this is still ambiguous - the *result* type isn't informing the
type of our *arguments*, which GHC is having a hard time with. If we try to
modify our functional dependency such that `c` determines either `a` or `b`,
then we get a complaint of illegal instance declarations - the "liberal coverage
condition fails" because the type `Filter rec` doesn't uniquely determine the
type `typ`. Fortunately, we can do this:

@
class SqlEquality a b c | c a -> b
@

This means that both the result and the LHS can determine the second argument.
With this, we can compile our first problem:

@
lol e = e ^. FooName ==. val 10
@

However, our second simple case requires a type annotation

@
lol' = FooName ==. 10
@

This form gives us a type error:

@
No instance for (SqlEquality (EntityField Foo Int) b0 c0)
@

The problem is that we know only `a`, and we cannot determine `c` or `b` from that.

We can introduce another functional dependency:

@
class SqlEquality a b c | a -> c, c a -> b where
@

And while this helps our type error, we still don't know enough to resolve it:

@
SqlEquality (EntityField Foo Int) b0 (Filter Foo)
@

What's... actually *really* strange here, is that adding a type signature to @lol'@ makes it work??

@
lol' :: Filter Foo
lol' = FooName ==. 10
@

This compiles just fine. Which is weird. Because the error message said that GHC
*knew* the result type already.

= Instance Local Fun Dep

The problem that GHC is having here is that we can't know that the polymorphic @10@ is supposed to have the same type as the record field. As a result, GHC isn't able to select the instance, since it hasn't yet proven that @a ~ b@ in @SqlEquality (EntityField rec a) b (Filter rec)@.

We can defer that by using a an equality constraint on the instance.

@
instance
    (PersistField typ, typ ~ typ')
  =>
    SqlEquality (EntityField rec typ) typ' (Filter rec)
  where
    (==.) = (Persist.==.)
@

This lets us omit the type signature on our problem expression, and now it works out just fine.

= OverloadedRecordDot

Honestly I'd expect the next thing to fail, but it actually works??

@
lol'' e = e.name ==. val 10
@

It just... works. Doesn't complain about a monomrophism restriction or anything. What on earth?? Throwing on an inferred signature gets weird, but providing `_` for a constraint allows us to know what's expected:

@
lol'' :: _ => _ -> _
@

The inferred constraint is @(SqlEquality a (SqlExpr (Value typ)) w, HasField "name" r a, PersistField typ, Num typ)@.

So, we can be pretty sure that polymorphic field access *should* work fine.

= Wrong Uses

If we write the following expression:

@
lol''' e = e ^. FooName ==. val "hello"
@

Then we get a type error:

@
/home/matt/Projects/esqueleto/test/Common/CompatSpec.hs:23:25: error:
    • No instance for (SqlEquality
                         (SqlExpr (Value Int))
                         (SqlExpr (Value String))
                         (SqlExpr (Value Bool)))
        arising from a use of ‘==.’
    • In the expression: e ^. FooName ==. val "hello"
      In an equation for ‘lol'''’:
          lol''' e = e ^. FooName ==. val "hello"
   |
23 | lol''' e = e ^. FooName ==. val "hello"
   |                         ^^^

@

I'm not thrilled with this, but it's reasonably close. Can we use another constraint equality trick?

Yes, we can -

@
instance
    (PersistField a, a ~ b)
  =>
    SqlEquality (SqlExpr (Value a)) (SqlExpr (Value b)) (SqlExpr (Value Bool))
  where
    (==.) = (Esqueleto.==.)
@

This gives us a more regular type error.

@
/home/matt/Projects/esqueleto/test/Common/CompatSpec.hs:24:25: error:
    • Couldn't match type ‘Int’ with ‘[Char]’
        arising from a use of ‘==.’
    • In the expression: e ^. FooName ==. val "hello"
      In an equation for ‘lol'''’:
          lol''' e = e ^. FooName ==. val "hello"
   |
24 | lol''' e = e ^. FooName ==. val "hello"
   |                         ^^^
@

This isn't quite as nice as what the regular esqueleto operator gives you, which actually refers to the data constructor:

@
/home/matt/Projects/esqueleto/test/Common/CompatSpec.hs:26:18: error:
    • Couldn't match type ‘[Char]’ with ‘Int’
        arising from a use of ‘FooName’
    • In the second argument of ‘(^.)’, namely ‘FooName’
      In the first argument of ‘(Esqueleto.==.)’, namely ‘e ^. FooName’
      In the expression: e ^. FooName Esqueleto.==. val "hello"
   |
26 | lol'''' e = e ^. FooName Esqueleto.==. val "hello"
   |                  ^^^^^^^
@

= Mismatched Uses

Let's say we make a mistake and write @FooName ==. val 10@. This should break.

@
/home/matt/Projects/esqueleto/test/Common/CompatSpec.hs:28:18: error:
    • Couldn't match type ‘Int’ with ‘SqlExpr (Value typ0)’
        arising from a use of ‘==.’
    • In the expression: FooName ==. val 10
      In an equation for ‘broken’: broken = FooName ==. val 10
   |
28 | broken = FooName ==. val 10
   |                  ^^^
@

This isn't a super helpful type error. It vaguely points at what is wrong, but isn't as clear as it could be. We can provide a better error message by using GHC's type error facilities:

@
type family NotSqlExpr typ :: Constraint where
    NotSqlExpr (SqlExpr _) = TypeError ('Text "No")
    NotSqlExpr _ = ()

instance
    (PersistField typ, typ ~ typ', NotSqlExpr typ')
  =>
    SqlEquality (EntityField rec typ) typ' (Filter rec)
  where
    (==.) = (Persist.==.)

/home/matt/Projects/esqueleto/test/Common/CompatSpec.hs:28:18: error:
    • No
    • In the expression: FooName ==. val 10
      In an equation for ‘broken’: broken = FooName ==. val 10
   |
28 | broken = FooName ==. val 10
   |                  ^^^
@

Neat - let's tidy up that error message and we're solid.

= Another errant expression

I kind of feel like the literals should be allowed. The following gives an error:

@
/home/matt/Projects/esqueleto/test/Common/CompatSpec.hs:29:29: error:
    • No instance for (Num (SqlExpr (Value Int)))
        arising from the literal ‘10’
    • In the second argument of ‘(==.)’, namely ‘10’
      In the expression: e ^. FooName ==. 10
      In an equation for ‘broken’: broken e = e ^. FooName ==. 10
   |
29 | broken e = e ^. FooName ==. 10
   |                             ^^
@

But maybe we should have the passthrough instance so it works.

I'm pretty happy with this as a proof of concept, and I think I can support extending this pattern.

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
class SqlEquality a b c | a -> c, c a -> b where
    (==.) :: a -> b -> c

type family NotSqlExpr rec typ' typ :: Constraint where
    NotSqlExpr rec typ' (SqlExpr (Value typ)) =
        TypeError (NotSqlExprMessage rec typ')
    NotSqlExpr _ _ _ =
        ()

type NotSqlExprMessage rec typ =
    'Text "You used a bare `" ':<>: 'ShowType (EntityField rec typ)
    ':<>: 'Text "` field."
    ':$$: 'Text "If you're writing a Persistent expression, you don't need to use `val`."
    ':$$: 'Text "If you're writing an esqueleto expression, you need to project from a "
    ':$$: 'Text "table variable, like: e ^. FooName"

instance
    (PersistField typ, typ ~ typ', NotSqlExpr rec typ typ')
  =>
    SqlEquality (EntityField rec typ) typ' (Filter rec)
  where
    (==.) = (Persist.==.)

instance
    (PersistField a, a ~ b)
  =>
    SqlEquality (SqlExpr (Value a)) (SqlExpr (Value b)) (SqlExpr (Value Bool))
  where
    (==.) = (Esqueleto.==.)
