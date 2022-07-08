{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.ToAliasReference
    where

import Data.Coerce
import Database.Esqueleto.Internal.Internal hiding (From, from, on)
import Database.Esqueleto.Internal.PersistentImport

{-# DEPRECATED ToAliasReferenceT "This type alias doesn't do anything. Please delete it. Will be removed in the next release." #-}
type ToAliasReferenceT a = a

-- more tedious tuple magic
class ToAliasReference a a' | a -> a' where
    toAliasReference :: Ident -> a -> SqlQuery a'

instance ToAliasReference (SqlExpr_ ctx (Value a)) (SqlExpr_ ValueContext (Value a)) where
    toAliasReference aliasSource (ERaw m _)
      | Just alias <- sqlExprMetaAlias m = pure $ ERaw m{sqlExprMetaIsReference = True} $ \_ info ->
          (useIdent info aliasSource <> "." <> useIdent info alias, [])
    toAliasReference _ e = pure $ coerce e

instance ToAliasReference (SqlExpr_ ctx (Entity a)) (SqlExpr_ ValueContext (Entity a)) where
    toAliasReference aliasSource (ERaw m _)
      | Just _ <- sqlExprMetaAlias m =
          pure $ ERaw m{sqlExprMetaIsReference = True} $ \_ info ->
            (useIdent info aliasSource, [])
    toAliasReference _ e = pure $ coerce e

instance ToAliasReference (SqlExpr_ ctx (Maybe (Entity a))) (SqlExpr_ ValueContext (Maybe (Entity a))) where
    toAliasReference aliasSource e =
        let maybelizeExpr :: SqlExpr_ ctx (Maybe (Entity a)) -> SqlExpr_ ctx (Entity a)
            maybelizeExpr = coerce
            unmaybelizeExpr :: SqlExpr_ ctx (Entity a) -> SqlExpr_ ctx (Maybe (Entity a))
            unmaybelizeExpr = coerce
        in
        unmaybelizeExpr <$> toAliasReference aliasSource (maybelizeExpr e)


instance (ToAliasReference a a', ToAliasReference b b') => ToAliasReference (a, b) (a', b') where
    toAliasReference ident (a,b) = (,) <$> toAliasReference ident a <*> toAliasReference ident b

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         ) => ToAliasReference (a,b,c) (a',b',c') where
    toAliasReference ident x = fmap to3 $ toAliasReference ident $ from3 x

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         ) => ToAliasReference (a,b,c,d) (a',b',c',d') where
    toAliasReference ident x = fmap to4 $ toAliasReference ident $ from4 x

    {--
instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         , ToAliasReference d
         , ToAliasReference e
         ) => ToAliasReference (a,b,c,d,e) where
    toAliasReference ident x = fmap to5 $ toAliasReference ident $ from5 x

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         , ToAliasReference d
         , ToAliasReference e
         , ToAliasReference f
         ) => ToAliasReference (a,b,c,d,e,f) where
    toAliasReference ident x = to6 <$> (toAliasReference ident $ from6 x)

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         , ToAliasReference d
         , ToAliasReference e
         , ToAliasReference f
         , ToAliasReference g
         ) => ToAliasReference (a,b,c,d,e,f,g) where
    toAliasReference ident x = to7 <$> (toAliasReference ident $ from7 x)

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         , ToAliasReference d
         , ToAliasReference e
         , ToAliasReference f
         , ToAliasReference g
         , ToAliasReference h
         ) => ToAliasReference (a,b,c,d,e,f,g,h) where
    toAliasReference ident x = to8 <$> (toAliasReference ident $ from8 x)
--}
