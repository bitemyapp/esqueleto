{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.From.SqlSetOperation
        where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Writer as W
import Database.Esqueleto.Experimental.From
import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Experimental.ToAliasReference
import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)
import Database.Esqueleto.Internal.PersistentImport (DBName(..))

data SqlSetOperation a
    = SqlSetUnion (SqlSetOperation a) (SqlSetOperation a)
    | SqlSetUnionAll (SqlSetOperation a) (SqlSetOperation a)
    | SqlSetExcept (SqlSetOperation a) (SqlSetOperation a)
    | SqlSetIntersect (SqlSetOperation a) (SqlSetOperation a)
    | SelectQueryP NeedParens (SqlQuery a)

runSetOperation :: (SqlSelect a r, ToAlias a, ToAliasReference a)
                => SqlSetOperation a -> SqlQuery (a, FromClause)
runSetOperation operation = do
    (aliasedOperation, ret) <- aliasQueries operation
    ident <- newIdentFor (DBName "u")
    ref <- toAliasReference ident ret
    pure (ref, FromQuery ident (operationToSql aliasedOperation) NormalSubQuery)

  where
    aliasQueries o =
        case o of
            SelectQueryP p q -> do
                (ret, sideData) <- Q $ W.censor (\_ -> mempty) $ W.listen $ unQ q
                prevState <- Q $ lift S.get
                aliasedRet <- toAlias ret
                Q $ lift $ S.put prevState
                let p' =
                      case p of
                        Parens -> Parens
                        Never ->
                          if (sdLimitClause sideData) /= mempty
                              || length (sdOrderByClause sideData) > 0 then
                            Parens
                          else
                            Never
                pure (SelectQueryP p' $ Q $ W.WriterT $ pure (aliasedRet, sideData), aliasedRet)
            SqlSetUnion     o1 o2 -> do
                (o1', ret) <- aliasQueries o1
                (o2', _  ) <- aliasQueries o2
                pure (SqlSetUnion o1' o2', ret)
            SqlSetUnionAll  o1 o2 -> do
                (o1', ret) <- aliasQueries o1
                (o2', _  ) <- aliasQueries o2
                pure (SqlSetUnionAll o1' o2', ret)
            SqlSetExcept    o1 o2 -> do
                (o1', ret) <- aliasQueries o1
                (o2', _  ) <- aliasQueries o2
                pure (SqlSetExcept o1' o2', ret)
            SqlSetIntersect o1 o2 -> do
                (o1', ret) <- aliasQueries o1
                (o2', _  ) <- aliasQueries o2
                pure (SqlSetIntersect o1' o2', ret)

    operationToSql o info =
        case o of
            SelectQueryP p q  ->
                let (builder, values) = toRawSql SELECT info q
                in (parensM p builder, values)
            SqlSetUnion     o1 o2 -> doSetOperation "UNION"     info o1 o2
            SqlSetUnionAll  o1 o2 -> doSetOperation "UNION ALL" info o1 o2
            SqlSetExcept    o1 o2 -> doSetOperation "EXCEPT"    info o1 o2
            SqlSetIntersect o1 o2 -> doSetOperation "INTERSECT" info o1 o2

    doSetOperation operationText info o1 o2 =
        let (q1, v1) = operationToSql o1 info
            (q2, v2) = operationToSql o2 info
        in (q1 <> " " <> operationText <> " " <> q2, v1 <> v2)


{-# DEPRECATED Union "/Since: 3.4.0.0/ - Use the 'union_' function instead of the 'Union' data constructor" #-}
data Union a b = a `Union` b

-- | @UNION@ SQL set operation. Can be used as an infix function between 'SqlQuery' values.
union_ :: a -> b -> Union a b
union_ = Union

{-# DEPRECATED UnionAll "/Since: 3.4.0.0/ - Use the 'unionAll_' function instead of the 'UnionAll' data constructor" #-}
data UnionAll a b = a `UnionAll` b

-- | @UNION@ @ALL@ SQL set operation. Can be used as an infix function between 'SqlQuery' values.
unionAll_ :: a -> b -> UnionAll a b
unionAll_ = UnionAll

{-# DEPRECATED Except "/Since: 3.4.0.0/ - Use the 'except_' function instead of the 'Except' data constructor" #-}
data Except a b = a `Except` b

-- | @EXCEPT@ SQL set operation. Can be used as an infix function between 'SqlQuery' values.
except_ :: a -> b -> Except a b
except_ = Except

{-# DEPRECATED Intersect "/Since: 3.4.0.0/ - Use the 'intersect_' function instead of the 'Intersect' data constructor" #-}
data Intersect a b = a `Intersect` b

-- | @INTERSECT@ SQL set operation. Can be used as an infix function between 'SqlQuery' values.
intersect_ :: a -> b -> Intersect a b
intersect_ = Intersect

class SetOperationT a ~ b => ToSetOperation a b | a -> b where
    type SetOperationT a
    toSetOperation :: a -> SqlSetOperation b
instance ToSetOperation (SqlSetOperation a) a where
    type SetOperationT (SqlSetOperation a) = a
    toSetOperation = id
instance ToSetOperation (SqlQuery a) a where
    type SetOperationT (SqlQuery a) = a
    toSetOperation = SelectQueryP Never
instance (ToSetOperation a c, ToSetOperation b c) => ToSetOperation (Union a b) c where
    type SetOperationT (Union a b) = SetOperationT a
    toSetOperation (Union a b) = SqlSetUnion (toSetOperation a) (toSetOperation b)
instance (ToSetOperation a c, ToSetOperation b c) => ToSetOperation (UnionAll a b) c where
    type SetOperationT (UnionAll a b) = SetOperationT a
    toSetOperation (UnionAll a b) = SqlSetUnionAll (toSetOperation a) (toSetOperation b)
instance (ToSetOperation a c, ToSetOperation b c) => ToSetOperation (Except a b) c where
    type SetOperationT (Except a b) = SetOperationT a
    toSetOperation (Except a b) = SqlSetExcept (toSetOperation a) (toSetOperation b)
instance (ToSetOperation a c, ToSetOperation b c) => ToSetOperation (Intersect a b) c where
    type SetOperationT (Intersect a b) = SetOperationT a
    toSetOperation (Intersect a b) = SqlSetIntersect (toSetOperation a) (toSetOperation b)

{-# DEPRECATED SelectQuery "/Since: 3.4.0.0/ - It is no longer necessary to tag 'SqlQuery' values with @SelectQuery@" #-}
pattern SelectQuery :: SqlQuery a -> SqlSetOperation a
pattern SelectQuery q = SelectQueryP Never q

instance
    ( SqlSelect c r
    , ToAlias c
    , ToAliasReference c
    , ToSetOperation a c
    , ToSetOperation b c
    , c ~ SetOperationT a
    )
  =>
    From (Union a b)
  where
    type FromT (Union a b) = SetOperationT a
    runFrom u = runSetOperation $ toSetOperation u

instance
    ( SqlSelect c r
    , ToAlias c
    , ToAliasReference c
    , ToSetOperation a c
    , ToSetOperation b c
    , c ~ SetOperationT a
    )
  =>
    From (UnionAll a b)
  where
    type FromT (UnionAll a b) = SetOperationT a
    runFrom u = runSetOperation $ toSetOperation u

instance
    ( SqlSelect c r
    , ToAlias c
    , ToAliasReference c
    , ToSetOperation a c
    , ToSetOperation b c
    , c ~ SetOperationT a
    )
  =>
    From (Intersect a b)
  where
    type FromT (Intersect a b) = SetOperationT a
    runFrom u = runSetOperation $ toSetOperation u

instance
    ( SqlSelect c r
    , ToAlias c
    , ToAliasReference c
    , ToSetOperation a c
    , ToSetOperation b c
    , c ~ SetOperationT a
    )
  =>
    From (Except a b)
  where
    type FromT (Except a b) = SetOperationT a
    runFrom u = runSetOperation $ toSetOperation u

instance (SqlSelect a r, ToAlias a, ToAliasReference a) => From (SqlSetOperation a) where
    type FromT (SqlSetOperation a) = a
    -- If someone uses just a plain SelectQuery it should behave like a normal subquery
    runFrom (SelectQueryP _ subquery) = fromSubQuery NormalSubQuery subquery
    -- Otherwise use the SqlSetOperation
    runFrom u = runSetOperation $ toSetOperation u
