{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Esqueleto.Experimental.ToMaybe
    where

import Database.Esqueleto.Internal.Internal hiding (From(..), from, on)
import Database.Esqueleto.Internal.PersistentImport (Entity(..))

type family Nullable a where
    Nullable (Maybe a) = a
    Nullable a =  a

class ToMaybe a where
    type ToMaybeT a
    toMaybe :: a -> ToMaybeT a

instance ToMaybe (SqlExpr (Maybe a)) where
    type ToMaybeT (SqlExpr (Maybe a)) = SqlExpr (Maybe a)
    toMaybe = id

instance ToMaybe (SqlExpr (Entity a)) where
    type ToMaybeT (SqlExpr (Entity a)) = SqlExpr (Maybe (Entity a))
    toMaybe (ERaw f m) = (ERaw f m)

instance ToMaybe (SqlExpr (Value a)) where
    type ToMaybeT (SqlExpr (Value a)) = SqlExpr (Value (Maybe (Nullable a)))
    toMaybe = veryUnsafeCoerceSqlExprValue


instance (ToMaybe a, ToMaybe b) => ToMaybe (a,b) where
    type ToMaybeT (a, b) = (ToMaybeT a, ToMaybeT b)
    toMaybe (a, b) = (toMaybe a, toMaybe b)

instance ( ToMaybe a , ToMaybe b , ToMaybe c) => ToMaybe (a,b,c) where
    type ToMaybeT (a, b, c) = (ToMaybeT a, ToMaybeT b, ToMaybeT c)
    toMaybe = to3 . toMaybe . from3

instance ( ToMaybe a , ToMaybe b , ToMaybe c , ToMaybe d) => ToMaybe (a,b,c,d) where
    type ToMaybeT (a, b, c, d) = (ToMaybeT a, ToMaybeT b, ToMaybeT c, ToMaybeT d)
    toMaybe = to4 . toMaybe . from4

instance ( ToMaybe a , ToMaybe b , ToMaybe c , ToMaybe d , ToMaybe e) => ToMaybe (a,b,c,d,e) where
    type ToMaybeT (a, b, c, d, e) = (ToMaybeT a, ToMaybeT b, ToMaybeT c, ToMaybeT d, ToMaybeT e)
    toMaybe = to5 . toMaybe . from5

instance ( ToMaybe a
         , ToMaybe b
         , ToMaybe c
         , ToMaybe d
         , ToMaybe e
         , ToMaybe f
         ) => ToMaybe (a,b,c,d,e,f) where
    type ToMaybeT (a, b, c, d, e, f) = (ToMaybeT a, ToMaybeT b, ToMaybeT c, ToMaybeT d, ToMaybeT e, ToMaybeT f)
    toMaybe = to6 . toMaybe . from6

instance ( ToMaybe a
         , ToMaybe b
         , ToMaybe c
         , ToMaybe d
         , ToMaybe e
         , ToMaybe f
         , ToMaybe g
         ) => ToMaybe (a,b,c,d,e,f,g) where
    type ToMaybeT (a, b, c, d, e, f, g) = (ToMaybeT a, ToMaybeT b, ToMaybeT c, ToMaybeT d, ToMaybeT e, ToMaybeT f, ToMaybeT g)
    toMaybe = to7 . toMaybe . from7

instance ( ToMaybe a
         , ToMaybe b
         , ToMaybe c
         , ToMaybe d
         , ToMaybe e
         , ToMaybe f
         , ToMaybe g
         , ToMaybe h
         ) => ToMaybe (a,b,c,d,e,f,g,h) where
     type ToMaybeT (a, b, c, d, e, f, g, h) = (ToMaybeT a, ToMaybeT b, ToMaybeT c, ToMaybeT d, ToMaybeT e, ToMaybeT f, ToMaybeT g, ToMaybeT h)
     toMaybe = to8 . toMaybe . from8

