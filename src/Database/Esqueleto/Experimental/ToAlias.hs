{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Esqueleto.Experimental.ToAlias
    where

import Database.Esqueleto.Internal.Internal hiding (From, from, on)
import Database.Esqueleto.Internal.PersistentImport

{-# DEPRECATED ToAliasT "This type alias doesn't do anything. Please delete it. Will be removed in the next release." #-}
type ToAliasT a = a

-- Tedious tuple magic
class ToAlias a where
    toAlias :: a -> SqlQuery a

instance ToAlias (SqlExpr (Value a)) where
    toAlias e@(ERaw m f)
      | Just _ <- sqlExprMetaAlias m, not (sqlExprMetaIsReference m) = pure e
      | otherwise = do
            ident <- newIdentFor (DBName "v")
            pure $ ERaw noMeta{sqlExprMetaAlias = Just ident} f

instance ToAlias (SqlExpr (Entity a)) where
    toAlias (ERaw m f) = do
       ident <- newIdentFor (DBName "v")
       pure $ ERaw m{sqlExprMetaIsReference = False, sqlExprMetaAlias = Just ident} f

instance ToAlias (SqlExpr (Maybe (Entity a))) where
    -- FIXME: Code duplication because the compiler doesnt like half final encoding
    toAlias (ERaw m f) = do
       ident <- newIdentFor (DBName "v")
       pure $ ERaw m{sqlExprMetaIsReference = False, sqlExprMetaAlias = Just ident} f

instance (ToAlias a, ToAlias b) => ToAlias (a,b) where
    toAlias (a,b) = (,) <$> toAlias a <*> toAlias b

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         ) => ToAlias (a,b,c) where
    toAlias x = to3 <$> (toAlias $ from3 x)

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         , ToAlias d
         ) => ToAlias (a,b,c,d) where
    toAlias x = to4 <$> (toAlias $ from4 x)

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         , ToAlias d
         , ToAlias e
         ) => ToAlias (a,b,c,d,e) where
    toAlias x = to5 <$> (toAlias $ from5 x)

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         , ToAlias d
         , ToAlias e
         , ToAlias f
         ) => ToAlias (a,b,c,d,e,f) where
    toAlias x = to6 <$> (toAlias $ from6 x)

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         , ToAlias d
         , ToAlias e
         , ToAlias f
         , ToAlias g
         ) => ToAlias (a,b,c,d,e,f,g) where
    toAlias x = to7 <$> (toAlias $ from7 x)

instance ( ToAlias a
         , ToAlias b
         , ToAlias c
         , ToAlias d
         , ToAlias e
         , ToAlias f
         , ToAlias g
         , ToAlias h
         ) => ToAlias (a,b,c,d,e,f,g,h) where
    toAlias x = to8 <$> (toAlias $ from8 x)
