{-# LANGUAGE CPP
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , MultiParamTypeClasses
           , TypeOperators
           , UndecidableInstances
           , OverloadedStrings
 #-}

module Database.Esqueleto.Experimental where

import qualified Control.Monad.Trans.Writer as W
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.Class (lift)
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif
import qualified Data.Monoid as Monoid
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
          , SqlSelect(..)
          , Mode(..)
          , toRawSql
          , Ident(..)
          , to3, to4, to5, to6, to7, to8
          , from3, from4, from5, from6, from7, from8
          )

data (:&) a b = a :& b
infixl 2 :&

data SqlSetOperation a =
    Union (SqlSetOperation a) (SqlSetOperation a)
  | UnionAll (SqlSetOperation a) (SqlSetOperation a)
  | Except (SqlSetOperation a) (SqlSetOperation a)
  | Intersect (SqlSetOperation a) (SqlSetOperation a)
  | SelectQuery (SqlQuery a)

data From a where 
  Table         :: PersistEntity ent => From (SqlExpr (Entity ent))
  SubQuery      :: (SqlSelect a' r, SqlSelect a'' r', ToAlias a a', ToAliasReference a' a'')
                => SqlQuery a
                -> From a''
  SqlSetOperation   :: (SqlSelect a' r, ToAlias a a', ToAliasReference a' a'') 
                => SqlSetOperation a
                -> From a''
  InnerJoinFrom :: From a 
                -> (From b, (a :& b) -> SqlExpr (Value Bool)) 
                -> From (a :& b)
  CrossJoinFrom :: From a 
                -> From b 
                -> From (a :& b)
  LeftJoinFrom  :: ToMaybe b mb
                => From a 
                -> (From b, (a :& mb) -> SqlExpr (Value Bool))
                -> From (a :& mb)
  RightJoinFrom :: ToMaybe a ma 
                => From a 
                -> (From b, (ma :& b) -> SqlExpr (Value Bool)) 
                -> From (ma :& b)
  FullJoinFrom  :: (ToMaybe a ma, ToMaybe b mb)
                => From a 
                -> (From b, (ma :& mb) -> SqlExpr (Value Bool))
                -> From (ma :& mb)

on :: ToFrom a a' => a -> b -> (a, b)
on = (,)
infix 9 `on`

{-- Type class magic to allow the use of the `InnerJoin` family of data constructors in from --}
class ToFrom a b | a -> b where
  toFrom :: a -> From b

instance ToFrom (From a) a where
  toFrom = id

{-- Potentially allow the passing of queries directly 
instance (SqlSelect a'' r', ToAlias a a', ToAliasReference a' a'') 
       => ToFrom (SqlQuery a) a'' where
  toFrom q = SubQuery q
--}
instance (SqlSelect a' r, SqlSelect a'' r', ToAlias a a', ToAliasReference a' a'') 
       => ToFrom (SqlSetOperation a) a'' where
  -- If someone uses just a plain SelectQuery it should behave like a normal subquery
  toFrom (SelectQuery q) = SubQuery q 
  -- Otherwise use the SqlSetOperation
  toFrom q = SqlSetOperation q

instance (ToFrom a a', ToFrom b b', ToMaybe b' mb) =>
          ToFrom (LeftOuterJoin 
                        a
                        (b, (a' :& mb) -> SqlExpr (Value Bool))
                      ) (a' :& mb) where
  toFrom (LeftOuterJoin lhs (rhs, on')) = LeftJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a a', ToFrom b b', ToMaybe a' ma, ToMaybe b' mb) =>
          ToFrom (FullOuterJoin 
                        a
                        (b, (ma :& mb) -> SqlExpr (Value Bool))
                      ) (ma :& mb) where
  toFrom (FullOuterJoin lhs (rhs, on')) = FullJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a a', ToFrom b b', ToMaybe a' ma) =>
          ToFrom (RightOuterJoin 
                        a
                        (b, (ma :& b') -> SqlExpr (Value Bool))
                      ) (ma :& b') where
  toFrom (RightOuterJoin lhs (rhs, on')) = RightJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a a', ToFrom b b') 
       => ToFrom (InnerJoin a (b, (a' :& b') -> SqlExpr (Value Bool))) (a' :& b') where
  toFrom (InnerJoin lhs (rhs, on')) = InnerJoinFrom (toFrom lhs) (toFrom rhs, on')

instance (ToFrom a a', ToFrom b b') 
       => ToFrom (CrossJoin a b) (a' :& b') where
  toFrom (CrossJoin lhs rhs) = CrossJoinFrom (toFrom lhs) (toFrom rhs)

class ToMaybe a b where
  toMaybe :: a -> b 

instance ToMaybe (SqlExpr (Maybe a)) (SqlExpr (Maybe a)) where
  toMaybe = id

instance ToMaybe (SqlExpr (Entity a)) (SqlExpr (Maybe (Entity a))) where
  toMaybe = EMaybe

instance (ToMaybe a a', ToMaybe b b') => ToMaybe (a :& b) (a' :& b') where
  toMaybe (a :& b) = (toMaybe a :& toMaybe b)

instance (ToMaybe a a', ToMaybe b b') => ToMaybe (a,b) (a',b') where
  toMaybe (a, b) = (toMaybe a, toMaybe b)

instance ( ToMaybe a a'
         , ToMaybe b b'
         , ToMaybe c c'
         ) => ToMaybe (a,b,c) (a',b',c') where
  toMaybe = to3 . toMaybe . from3

instance ( ToMaybe a a'
         , ToMaybe b b'
         , ToMaybe c c'
         , ToMaybe d d'
         ) => ToMaybe (a,b,c,d) (a',b',c',d') where
  toMaybe = to4 . toMaybe . from4

instance ( ToMaybe a a'
         , ToMaybe b b'
         , ToMaybe c c'
         , ToMaybe d d'
         , ToMaybe e e'
         ) => ToMaybe (a,b,c,d,e) (a',b',c',d',e') where
  toMaybe = to5 . toMaybe . from5

instance ( ToMaybe a a'
         , ToMaybe b b'
         , ToMaybe c c'
         , ToMaybe d d'
         , ToMaybe e e'
         , ToMaybe f f'
         ) => ToMaybe (a,b,c,d,e,f) (a',b',c',d',e',f') where
  toMaybe = to6 . toMaybe . from6

instance ( ToMaybe a a'
         , ToMaybe b b'
         , ToMaybe c c'
         , ToMaybe d d'
         , ToMaybe e e'
         , ToMaybe f f'
         , ToMaybe g g'
         ) => ToMaybe (a,b,c,d,e,f,g) (a',b',c',d',e',f',g') where
  toMaybe = to7 . toMaybe . from7

instance ( ToMaybe a a'
         , ToMaybe b b'
         , ToMaybe c c'
         , ToMaybe d d'
         , ToMaybe e e'
         , ToMaybe f f'
         , ToMaybe g g'
         , ToMaybe h h'
         ) => ToMaybe (a,b,c,d,e,f,g,h) (a',b',c',d',e',f',g',h') where
  toMaybe = to8 . toMaybe . from8


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
      runFrom (SubQuery subquery) = do
          -- We want to update the IdentState without writing the query to side data
          (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ subquery
          aliasedValue <- toAlias ret
          -- Make a fake query with the aliased results, this allows us to ensure that the query is only run once
          let aliasedQuery = Q $ W.WriterT $ pure (aliasedValue, sideData)
          -- Add the FromQuery that renders the subquery to our side data
          subqueryAlias <- newIdentFor (DBName "q")
          -- Pass the aliased results of the subquery to the outer query
          -- create aliased references from the outer query results (e.g value from subquery will be `subquery`.`value`), 
          -- this is probably overkill as the aliases should already be unique but seems to be good practice.
          ref <- toAliasReference subqueryAlias aliasedValue 
          pure (ref , FromQuery subqueryAlias (\info -> toRawSql SELECT info aliasedQuery))

      runFrom (SqlSetOperation operation) = do
          (aliasedOperation, ret) <- aliasQueries operation
          ident <- newIdentFor (DBName "u")
          ref <- toAliasReference ident ret 
          pure (ref, FromQuery ident $ operationToSql aliasedOperation)

          where
            aliasQueries o =
              case o of
                SelectQuery q -> do 
                  (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ q
                  prevState <- Q $ lift S.get
                  aliasedRet <- toAlias ret
                  Q $ lift $ S.put prevState
                  pure (SelectQuery $ Q $ W.WriterT $ pure (aliasedRet, sideData), aliasedRet)
                Union     o1 o2 -> do
                  (o1', ret) <- aliasQueries o1
                  (o2', _  ) <- aliasQueries o2
                  pure (Union o1' o2', ret)
                UnionAll  o1 o2 -> do 
                  (o1', ret) <- aliasQueries o1
                  (o2', _  ) <- aliasQueries o2
                  pure (UnionAll o1' o2', ret)
                Except    o1 o2 -> do 
                  (o1', ret) <- aliasQueries o1
                  (o2', _  ) <- aliasQueries o2
                  pure (Except o1' o2', ret)
                Intersect o1 o2 -> do 
                  (o1', ret) <- aliasQueries o1
                  (o2', _  ) <- aliasQueries o2
                  pure (Intersect o1' o2', ret)

            operationToSql o info =
              case o of
                SelectQuery q   -> toRawSql SELECT info q
                Union     o1 o2 -> doSetOperation "UNION"     info o1 o2
                UnionAll  o1 o2 -> doSetOperation "UNION ALL" info o1 o2
                Except    o1 o2 -> doSetOperation "EXCEPT"    info o1 o2
                Intersect o1 o2 -> doSetOperation "INTERSECT" info o1 o2

            doSetOperation operationText info o1 o2 =
                  let 
                    (q1, v1) = operationToSql o1 info
                    (q2, v2) = operationToSql o2 info
                  in (q1 <> " " <> operationText <> " " <> q2, v1 <> v2)


      runFrom (InnerJoinFrom leftPart (rightPart, on')) = do 
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = leftVal :& rightVal
        pure $ (ret, FromJoin leftFrom InnerJoinKind rightFrom (Just (on' ret)))
      runFrom (CrossJoinFrom leftPart rightPart) = do 
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = leftVal :& rightVal
        pure $ (ret, FromJoin leftFrom CrossJoinKind rightFrom Nothing)
      runFrom (LeftJoinFrom leftPart (rightPart, on')) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = leftVal :& (toMaybe rightVal)
        pure $ (ret, FromJoin leftFrom LeftOuterJoinKind rightFrom (Just (on' ret)))
      runFrom (RightJoinFrom leftPart (rightPart, on')) = do 
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = (toMaybe leftVal) :& rightVal
        pure $ (ret, FromJoin leftFrom RightOuterJoinKind rightFrom (Just (on' ret)))
      runFrom (FullJoinFrom leftPart (rightPart, on')) = do
        (leftVal, leftFrom) <- runFrom leftPart
        (rightVal, rightFrom) <- runFrom rightPart
        let ret = (toMaybe leftVal) :& (toMaybe rightVal)
        pure $ (ret, FromJoin leftFrom FullOuterJoinKind rightFrom (Just (on' ret)))

-- Tedious tuple magic
class ToAlias a b | a -> b where
  toAlias     :: a -> SqlQuery b

instance ToAlias (SqlExpr (Value a)) (SqlExpr (Value a)) where
  toAlias v@(EAliasedValue _ _) = pure v
  toAlias v = do 
    ident <- newIdentFor (DBName "v")
    pure $ EAliasedValue ident v

instance ToAlias (SqlExpr (Entity a)) (SqlExpr (Entity a)) where
  toAlias v@(EAliasedEntityReference _ _) = pure v
  toAlias v@(EAliasedEntity _ _) = pure v
  toAlias (EEntity tableIdent) = do 
    ident <- newIdentFor (DBName "v")
    pure $ EAliasedEntity ident tableIdent

instance ( ToAlias a a', ToAlias b b') => ToAlias (a,b) (a',b') where
  toAlias (a,b) = (,) <$> toAlias a <*> toAlias b

instance ( ToAlias a a'
         , ToAlias b b'
         , ToAlias c c'
         ) => ToAlias (a,b,c) (a',b',c') where
  toAlias x = to3 <$> (toAlias $ from3 x)

instance ( ToAlias a a'
         , ToAlias b b'
         , ToAlias c c'
         , ToAlias d d'
         ) => ToAlias (a,b,c,d) (a',b',c',d') where
  toAlias x = to4 <$> (toAlias $ from4 x)

instance ( ToAlias a a'
         , ToAlias b b'
         , ToAlias c c'
         , ToAlias d d'
         , ToAlias e e'
         ) => ToAlias (a,b,c,d,e) (a',b',c',d',e') where
  toAlias x = to5 <$> (toAlias $ from5 x)

instance ( ToAlias a a'
         , ToAlias b b'
         , ToAlias c c'
         , ToAlias d d'
         , ToAlias e e'
         , ToAlias f f'
         ) => ToAlias (a,b,c,d,e,f) (a',b',c',d',e',f') where
  toAlias x = to6 <$> (toAlias $ from6 x)

instance ( ToAlias a a'
         , ToAlias b b'
         , ToAlias c c'
         , ToAlias d d'
         , ToAlias e e'
         , ToAlias f f'
         , ToAlias g g'
         ) => ToAlias (a,b,c,d,e,f,g) (a',b',c',d',e',f',g') where
  toAlias x = to7 <$> (toAlias $ from7 x)

instance ( ToAlias a a'
         , ToAlias b b'
         , ToAlias c c'
         , ToAlias d d'
         , ToAlias e e'
         , ToAlias f f'
         , ToAlias g g'
         , ToAlias h h'
         ) => ToAlias (a,b,c,d,e,f,g,h) (a',b',c',d',e',f',g',h') where
  toAlias x = to8 <$> (toAlias $ from8 x)


-- more tedious tuple magic 
class ToAliasReference a b | a -> b where
  toAliasReference :: Ident -> a -> SqlQuery b

instance ToAliasReference (SqlExpr (Value a)) (SqlExpr (Value a)) where
  toAliasReference aliasSource (EAliasedValue aliasIdent _) = pure $ EValueReference aliasSource (\_ -> aliasIdent)
  toAliasReference _           v@(ERaw _ _)                 = toAlias v
  toAliasReference _           v@(ECompositeKey _)          = toAlias v
  toAliasReference _           v@(EValueReference _ _)      = pure v 

instance ToAliasReference (SqlExpr (Entity a)) (SqlExpr (Entity a)) where
  toAliasReference aliasSource (EAliasedEntity ident _) = pure $ EAliasedEntityReference aliasSource ident
  toAliasReference _ e@(EEntity _) = toAlias e 
  toAliasReference _ e@(EAliasedEntityReference _ _) = pure e

instance ( ToAliasReference a a', ToAliasReference b b') => ToAliasReference (a, b) ( a', b' ) where
  toAliasReference ident (a,b) = (,) <$> (toAliasReference ident a) <*> (toAliasReference ident b)

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         ) => ToAliasReference (a,b,c) ( a', b', c') where
  toAliasReference ident x = fmap to3 $ toAliasReference ident $ from3 x

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         ) => ToAliasReference (a,b,c,d) (a',b',c',d') where
  toAliasReference ident x = fmap to4 $ toAliasReference ident $ from4 x

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         ) => ToAliasReference (a,b,c,d,e) (a',b',c',d',e') where
  toAliasReference ident x = fmap to5 $ toAliasReference ident $ from5 x

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         ) => ToAliasReference (a,b,c,d,e,f) (a',b',c',d',e',f') where
  toAliasReference ident x = to6 <$> (toAliasReference ident $ from6 x)

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         , ToAliasReference g g'
         ) => ToAliasReference (a,b,c,d,e,f,g) (a',b',c',d',e',f',g') where
  toAliasReference ident x = to7 <$> (toAliasReference ident $ from7 x)

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         , ToAliasReference g g'
         , ToAliasReference h h'
         ) => ToAliasReference (a,b,c,d,e,f,g,h) (a',b',c',d',e',f',g',h') where
  toAliasReference ident x = to8 <$> (toAliasReference ident $ from8 x)
