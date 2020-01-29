{-# LANGUAGE FlexibleInstances 
           , FunctionalDependencies
           , GADTs
           , MultiParamTypeClasses
           , UndecidableInstances
 #-}

module Database.Esqueleto.Experimental where

import qualified Control.Monad.Trans.Writer as W
import Data.Proxy (Proxy(..))
import Database.Esqueleto.Internal.PersistentImport
import Database.Esqueleto.Internal.Internal 
          ( SqlExpr(..)
          , InnerJoin(..)
          , CrossJoin(..)
          , LeftOuterJoin(..)
          , RightOuterJoin(..)
          , FullOuterJoin(..)
          , FromClause(..)
          , SqlQuery(..)
          , SideData(..)
          , Value(..)
          , JoinKind(..)
          , newIdentFor
          )

data From a where 
  Table         :: PersistEntity ent => From (SqlExpr (Entity ent))
  InnerJoinFrom :: From a 
                -> (From b, (a,b) -> SqlExpr (Value Bool)) 
                -> From (a,b)
  CrossJoinFrom :: From a 
                -> From b 
                -> From (a,b)
  LeftJoinFrom  :: ToMaybe b mb
                => From a 
                -> (From b, (a, mb) -> SqlExpr (Value Bool))
                -> From (a, mb)
  RightJoinFrom :: ToMaybe a ma 
                => From a 
                -> (From b, (ma, b) -> SqlExpr (Value Bool)) 
                -> From (ma, b)
  FullJoinFrom  :: (ToMaybe a ma, ToMaybe b mb)
                => From a 
                -> (From b, (ma, mb) -> SqlExpr (Value Bool))
                -> From (ma, mb)

on :: ToFrom a a' => a -> b -> (a, b)
on = (,)
infix 9 `on`

{-- Type class magic to allow the use of the `InnerJoin` family of data constructors in from --}
class ToFrom a b | a -> b where
  toFrom :: a -> From b

instance ToFrom (From a) a where
  toFrom = id

instance (ToFrom a a', ToFrom b b', ToMaybe b' mb) =>
          ToFrom (LeftOuterJoin 
                        a
                        (b, (a', mb) -> SqlExpr (Value Bool))
                      ) (a', mb) where
  toFrom (LeftOuterJoin lhs (rhs, on')) = LeftJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a a', ToFrom b b', ToMaybe a' ma, ToMaybe b' mb) =>
          ToFrom (FullOuterJoin 
                        a
                        (b, (ma, mb) -> SqlExpr (Value Bool))
                      ) (ma, mb) where
  toFrom (FullOuterJoin lhs (rhs, on')) = FullJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a a', ToFrom b b', ToMaybe a' ma) =>
          ToFrom (RightOuterJoin 
                        a
                        (b, (ma, b') -> SqlExpr (Value Bool))
                      ) (ma, b') where
  toFrom (RightOuterJoin lhs (rhs, on')) = RightJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a a', ToFrom b b') 
       => ToFrom (InnerJoin a (b, (a',b') -> SqlExpr (Value Bool))) (a', b') where
  toFrom (InnerJoin lhs (rhs, on')) = InnerJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a a', ToFrom b b') 
       => ToFrom (CrossJoin a b) (a', b') where
  toFrom (CrossJoin lhs rhs) = CrossJoinFrom (toFrom lhs) (toFrom rhs)

class ToMaybe a b where
  toMaybe :: a -> b 

instance ToMaybe (SqlExpr (Maybe a)) (SqlExpr (Maybe a)) where
  toMaybe = id
instance ToMaybe (SqlExpr (Entity a)) (SqlExpr (Maybe (Entity a))) where
  toMaybe = EMaybe
instance (ToMaybe a a', ToMaybe b b') => ToMaybe (a,b) (a',b') where
  toMaybe (a,b) = (toMaybe a, toMaybe b) 
-- TODO: allow more sized tuples

from :: ToFrom a a' => a -> SqlQuery a'
from parts = do 
  (a, clause) <- runFrom $ toFrom parts
  Q $ W.tell mempty{sdFromClause=[clause]}
  pure a
    where
      runFrom :: From a -> SqlQuery (a, FromClause)
      runFrom e@Table = do 
        let ed = entityDef $ getVal e
        ident <- newIdentFor (entityDB ed)
        let entity = EEntity ident
        pure $ (entity, FromStart ident ed)
          where 
            getVal :: PersistEntity ent => From (SqlExpr (Entity ent)) -> Proxy ent
            getVal = const Proxy
      runFrom (InnerJoinFrom leftPart (rightPart, on')) = do 
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        pure $ ((leftVal, rightVal), FromJoin leftFrom InnerJoinKind rightFrom (Just (on' (leftVal, rightVal))))
      runFrom (CrossJoinFrom leftPart rightPart) = do 
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        pure $ ((leftVal, rightVal), FromJoin leftFrom CrossJoinKind rightFrom Nothing)
      runFrom (LeftJoinFrom leftPart (rightPart, on')) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        pure $ ((leftVal, toMaybe rightVal), FromJoin leftFrom LeftOuterJoinKind rightFrom (Just (on' (leftVal, toMaybe rightVal))))
      runFrom (RightJoinFrom leftPart (rightPart, on')) = do 
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        pure $ ((toMaybe leftVal, rightVal), FromJoin leftFrom RightOuterJoinKind rightFrom (Just (on' (toMaybe leftVal, rightVal))))
      runFrom (FullJoinFrom leftPart (rightPart, on')) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        pure $ ((toMaybe leftVal, toMaybe rightVal), FromJoin leftFrom FullOuterJoinKind rightFrom (Just (on' (toMaybe leftVal, toMaybe rightVal))))

