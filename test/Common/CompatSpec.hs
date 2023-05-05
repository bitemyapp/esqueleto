{-# language FlexibleContexts #-}
{-# language OverloadedLabels #-}
{-# language PartialTypeSignatures #-}
{-# language OverloadedRecordDot #-}

module Common.CompatSpec where

import Common.Test.Models
import Database.Esqueleto.Compat
import Database.Esqueleto.Experimental (val, (^.), SqlExpr, Value)
import qualified Database.Esqueleto.Experimental as Esqueleto
import Database.Persist.Sql (PersistField, PersistEntity, Filter, Entity)

lol e = e ^. FooName ==. val 10

wat = FooName ==. 10

lol' :: [_ ]
lol' = [PersonName ==. "asdf", PersonAge ==. Just 10]

lol'' :: _ => _ -> _
lol'' e = e.name ==. val 10

-- lol''' e = e ^. FooName ==. val "hello"

-- lol'''' e = e ^. FooName Esqueleto.==. val "hello"

-- broken = FooName ==. val 10
broken e = e ^. FooName ==. 10

-- lol'' = FooName ==. val 10

-- This expression only type checks if GHC knows about the input. Otherwise you
-- get a type error because it can't figure out what it's supposed to be.
--
-- Hmm. That's unfortunate. I'd like for any use of `val` or `SqlExpr` on either
-- side to cause it to infer `SqlExpr` on the other side, too.
--
-- Removing 'Foo' from the signature gives you a lot of ambiguous type variable
-- errors.
-- lol''' :: SqlExpr (Entity Foo) -> _
-- lol''' e = e.name ==. val 10

-- This is clearly not gonna type check
-- lol'''' = #name ==. 10

-- This one also doesn't type check - that the result type needs to be @'Filter'
-- 'Foo'@ doesn't help infer what the source is. GHC infers `SqlEquality a0 b0
-- (Filter Foo)`. I'd like it to know that `Filter Foo` means that we need to
-- have a `EntityField Foo typ` on the LHS...
-- lol'''' = [#name ==. 10, FooId ==. FooKey 10]
