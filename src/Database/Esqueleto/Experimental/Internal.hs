{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.Internal
    where

import qualified Control.Monad.Trans.Writer as W
import Data.Proxy
import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)
import Database.Esqueleto.Internal.PersistentImport

-- | 'FROM' clause, used to bring entities into scope.
--
-- Internally, this function uses the `From` datatype and the
-- `From` typeclass. Unlike the old `Database.Esqueleto.from`,
-- this does not take a function as a parameter, but rather
-- a value that represents a 'JOIN' tree constructed out of
-- instances of `From`. This implementation eliminates certain
-- types of runtime errors by preventing the construction of
-- invalid SQL (e.g. illegal nested-@from@).
from :: From a  => a -> SqlQuery (FromT a)
from parts = do
    (a, clause) <- runFrom parts
    Q $ W.tell mempty{sdFromClause=[clause]}
    pure a

class From a where
    type FromT a
    runFrom :: a -> SqlQuery (FromT a, FromClause)

-- | Data type for bringing a Table into scope in a JOIN tree
--
-- @
-- select $ from $ Table \@People
-- @
data Table a = Table

instance PersistEntity a => From (Table a) where
    type FromT (Table a) = SqlExpr (Entity a)
    runFrom e@Table = do
        let ed = entityDef $ getVal e
        ident <- newIdentFor (entityDB ed)
        let entity = EEntity ident
        pure $ (entity, FromStart ident ed)
          where
            getVal ::  Table ent -> Proxy ent
            getVal = const Proxy
