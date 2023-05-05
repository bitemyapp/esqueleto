{-# language FlexibleContexts #-}
{-# language OverloadedLabels #-}
{-# language PartialTypeSignatures #-}
{-# language OverloadedRecordDot #-}

module Common.CompatSpec where

import Common.Test.Models
import Database.Esqueleto.Compat
import Database.Esqueleto.Experimental (val, (^.), SqlExpr, Value)
import Database.Persist.Sql (PersistField, PersistEntity, Filter, Entity)

lol e = e ^. FooName ==. val 10

lol' = FooName ==. 10

-- This use gives a helpful type error message
-- lol'' = FooName ==. val 10

-- This expression only type checks if GHC knows about the input. Otherwise you
-- get a type error because it can't figure out what it's supposed to be.
--
-- Hmm. That's unfortunate. I'd like for any use of `val` or `SqlExpr` on either
-- side to cause it to infer `SqlExpr` on the other side, too.
--
-- Removing 'Foo' from the signature gives you a lot of ambiguous type variable
-- errors.
lol''' :: SqlExpr (Entity Foo) -> _
lol''' e = e.name ==. val 10

-- This is clearly not gonna type check
-- lol'''' = #name ==. 10

lol'''' = [#name ==. 10, FooId ==. FooKey 10]
