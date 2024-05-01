{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Experimental.ToAliasReference
    where

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
    toAliasReference _ e = pure $ veryUnsafeCoerceSqlExpr e

instance ToAliasReference (SqlExpr_ ctx (Entity a)) (SqlExpr_ ValueContext (Entity a)) where
    toAliasReference aliasSource (ERaw m _)
      | Just _ <- sqlExprMetaAlias m =
          pure $ ERaw m{sqlExprMetaIsReference = True} $ \_ info ->
            (useIdent info aliasSource, [])
    toAliasReference _ e = pure $ veryUnsafeCoerceSqlExpr e

instance ToAliasReference (SqlExpr_ ctx (Maybe (Entity a))) (SqlExpr_ ValueContext (Maybe (Entity a))) where
    toAliasReference aliasSource e =
        let maybelizeExpr :: SqlExpr_ ctx (Maybe (Entity a)) -> SqlExpr_ ctx (Entity a)
            maybelizeExpr = veryUnsafeCoerceSqlExpr
            unmaybelizeExpr :: SqlExpr_ ctx (Entity a) -> SqlExpr_ ctx (Maybe (Entity a))
            unmaybelizeExpr = veryUnsafeCoerceSqlExpr
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

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         , ToAliasReference g g'
         , ToAliasReference h h'
         , ToAliasReference i i'
         ) => ToAliasReference (a,b,c,d,e,f,g,h,i) (a',b',c',d',e',f',g',h',i') where
    toAliasReference ident x = to9 <$> (toAliasReference ident $ from9 x)

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         , ToAliasReference g g'
         , ToAliasReference h h'
         , ToAliasReference i i'
         , ToAliasReference j j'
         ) => ToAliasReference (a,b,c,d,e,f,g,h,i,j) (a',b',c',d',e',f',g',h',i',j') where
    toAliasReference ident x = to10 <$> (toAliasReference ident $ from10 x)

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         , ToAliasReference g g'
         , ToAliasReference h h'
         , ToAliasReference i i'
         , ToAliasReference j j'
         , ToAliasReference k k'
         ) => ToAliasReference (a,b,c,d,e,f,g,h,i,j,k) (a',b',c',d',e',f',g',h',i',j',k') where
    toAliasReference ident x = to11 <$> (toAliasReference ident $ from11 x)

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         , ToAliasReference g g'
         , ToAliasReference h h'
         , ToAliasReference i i'
         , ToAliasReference j j'
         , ToAliasReference k k'
         , ToAliasReference l l'
         ) => ToAliasReference (a,b,c,d,e,f,g,h,i,j,k,l) (a',b',c',d',e',f',g',h',i',j',k',l') where
    toAliasReference ident x = to12 <$> (toAliasReference ident $ from12 x)

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         , ToAliasReference g g'
         , ToAliasReference h h'
         , ToAliasReference i i'
         , ToAliasReference j j'
         , ToAliasReference k k'
         , ToAliasReference l l'
         , ToAliasReference m m'
         ) => ToAliasReference (a,b,c,d,e,f,g,h,i,j,k,l,m) (a',b',c',d',e',f',g',h',i',j',k',l',m') where
    toAliasReference ident x = to13 <$> (toAliasReference ident $ from13 x)

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         , ToAliasReference g g'
         , ToAliasReference h h'
         , ToAliasReference i i'
         , ToAliasReference j j'
         , ToAliasReference k k'
         , ToAliasReference l l'
         , ToAliasReference m m'
         , ToAliasReference n n'
         ) => ToAliasReference (a,b,c,d,e,f,g,h,i,j,k,l,m,n) (a',b',c',d',e',f',g',h',i',j',k',l',m',n') where
    toAliasReference ident x = to14 <$> (toAliasReference ident $ from14 x)

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         , ToAliasReference g g'
         , ToAliasReference h h'
         , ToAliasReference i i'
         , ToAliasReference j j'
         , ToAliasReference k k'
         , ToAliasReference l l'
         , ToAliasReference m m'
         , ToAliasReference n n'
         , ToAliasReference o o'
         ) => ToAliasReference (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) (a',b',c',d',e',f',g',h',i',j',k',l',m',n',o') where
    toAliasReference ident x = to15 <$> (toAliasReference ident $ from15 x)

instance ( ToAliasReference a a'
         , ToAliasReference b b'
         , ToAliasReference c c'
         , ToAliasReference d d'
         , ToAliasReference e e'
         , ToAliasReference f f'
         , ToAliasReference g g'
         , ToAliasReference h h'
         , ToAliasReference i i'
         , ToAliasReference j j'
         , ToAliasReference k k'
         , ToAliasReference l l'
         , ToAliasReference m m'
         , ToAliasReference n n'
         , ToAliasReference o o'
         , ToAliasReference p p'
         ) => ToAliasReference (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) (a',b',c',d',e',f',g',h',i',j',k',l',m',n',o',p') where
    toAliasReference ident x = to16 <$> (toAliasReference ident $ from16 x)
