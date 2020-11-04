{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Esqueleto.Experimental.ToAliasReference
    where

import Database.Esqueleto.Experimental.ToAlias
import Database.Esqueleto.Internal.Internal hiding (From, from, on)
import Database.Esqueleto.Internal.PersistentImport

{-# DEPRECATED ToAliasReferenceT "This type alias doesn't do anything. Please delete it. Will be removed in the next release." #-}
type ToAliasReferenceT a = a

-- more tedious tuple magic
class ToAliasReference a where
    toAliasReference :: Ident -> a -> SqlQuery a

instance ToAliasReference (SqlExpr (Value a)) where
    toAliasReference aliasSource (EAliasedValue aliasIdent _) = pure $ EValueReference aliasSource (\_ -> aliasIdent)
    toAliasReference _           v@(ERaw _ _)                 = toAlias v
    toAliasReference _           v@(ECompositeKey _)          = toAlias v
    toAliasReference s             (EValueReference _ b)      = pure $ EValueReference s b

instance ToAliasReference (SqlExpr (Entity a)) where
    toAliasReference aliasSource (EAliasedEntity ident _) = pure $ EAliasedEntityReference aliasSource ident
    toAliasReference _ e@(EEntity _) = toAlias e
    toAliasReference s   (EAliasedEntityReference _ b) = pure $ EAliasedEntityReference s b

instance ToAliasReference (SqlExpr (Maybe (Entity a))) where
    toAliasReference s (EMaybe e) = EMaybe <$> toAliasReference s e


instance (ToAliasReference a, ToAliasReference b) => ToAliasReference (a, b) where
    toAliasReference ident (a,b) = (,) <$> (toAliasReference ident a) <*> (toAliasReference ident b)

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         ) => ToAliasReference (a,b,c) where
    toAliasReference ident x = fmap to3 $ toAliasReference ident $ from3 x

instance ( ToAliasReference a
         , ToAliasReference b
         , ToAliasReference c
         , ToAliasReference d
         ) => ToAliasReference (a,b,c,d) where
    toAliasReference ident x = fmap to4 $ toAliasReference ident $ from4 x

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

