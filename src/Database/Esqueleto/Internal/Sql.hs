{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, GADTs, OverloadedStrings #-}
module Database.Esqueleto.Internal.Sql
  ( SqlQuery
  , select
  , toRawSelectSql
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow (first, (&&&))
import Control.Monad (ap)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Data.List (intersperse)
import Data.Monoid (Monoid(..), (<>))
import Database.Persist.EntityDef
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Internal (Connection(escapeName))
import Database.Persist.Store
import qualified Control.Monad.Supply as S
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Writer as W
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import Database.Esqueleto.Internal.Language


-- | SQL backend for 'Esqueleto' using 'SqlPersist'.
newtype SqlQuery a =
  Q { unQ :: W.WriterT SideData (S.Supply Ident) a }

instance Functor SqlQuery where
  fmap f = Q . fmap f . unQ

instance Monad SqlQuery where
  return  = Q . return
  m >>= f = Q (unQ m >>= unQ . f)

instance Applicative SqlQuery where
  pure  = return
  (<*>) = ap


-- | Side data written by 'SqlQuery'.
data SideData = SideData { sdFromClause    :: ![FromClause]
                         , sdWhereClause   :: !WhereClause
                         }

instance Monoid SideData where
  mempty = SideData mempty mempty
  SideData f w `mappend` SideData f' w' =
    SideData (f <> f') (w <> w')


-- | A part of a @FROM@ clause.
data FromClause  = From Ident EntityDef


-- | A complere @WHERE@ clause.
data WhereClause = Where (SqlExpr (Single Bool))
                 | NoWhere

instance Monoid WhereClause where
  mempty = NoWhere
  NoWhere  `mappend` w        = w
  w        `mappend` NoWhere  = w
  Where e1 `mappend` Where e2 = Where (e1 &&. e2)


-- | Identifier used for tables.
newtype Ident = I  TLB.Builder


-- | Infinite list of identifiers.
idents :: () -- ^ Avoid keeping everything in memory.
       -> [Ident]
idents _ =
  let alpha      = ['A'..'Z']
      letters 1  = map return alpha
      letters n  = (:) <$> alpha <*> letters (n-1)
      everything = concat (map letters [(1::Int)..])
  in map (I . TLB.fromString . ('T':)) everything


-- | An expression on the SQL backend.
data SqlExpr a where
  EEntity :: Ident -> SqlExpr (Entity val)
  ERaw    :: (Escape -> (TLB.Builder, [PersistValue])) -> SqlExpr (Single a)

type Escape = DBName -> TLB.Builder

instance Esqueleto SqlQuery SqlExpr SqlPersist where
  fromSingle = Q $ do
    ident <- S.supply
    let from_ = From ident $ entityDef (getVal ret)
        ret   = EEntity ident
        getVal :: SqlExpr (Entity val) -> val
        getVal = error "SqlQuery/getVal: never here"
    W.tell mempty { sdFromClause = [from_] }
    return ret

  where_ expr = Q $ W.tell mempty { sdWhereClause = Where expr }

  EEntity (I ident) ^. field = ERaw $ \esc -> (ident <> ("." <> name esc field), [])
      where name esc = esc . fieldDB . persistFieldDef

  val = ERaw . const . (,) "?" . return . toPersistValue

  not_ (ERaw f) = ERaw $ \esc -> let (b, vals) = f esc
                                 in ("NOT " <> parens b, vals)

  (==.) = binop " = "
  (>=.) = binop " >= "
  (>.)  = binop " > "
  (<=.) = binop " <= "
  (<.)  = binop " < "
  (!=.) = binop " != "
  (&&.) = binop " AND "
  (||.) = binop " OR "
  (+.)  = binop " + "
  (-.)  = binop " - "
  (/.)  = binop " / "
  (*.)  = binop " * "


fromDBName :: Connection -> DBName -> TLB.Builder
fromDBName conn = TLB.fromText . escapeName conn

binop :: TLB.Builder -> SqlExpr (Single a) -> SqlExpr (Single b) -> SqlExpr (Single c)
binop op (ERaw f1) (ERaw f2) = ERaw f
  where
    f esc = let (b1, vals1) = f1 esc
                (b2, vals2) = f2 esc
            in ( parens b1 <> op <> parens b2
               , vals1 <> vals2 )


-- | TODO
select :: ( SqlSelect a
          , RawSql (SqlSelectRet r)
          , MonadLogger m
          , MonadResourceBase m)
       => SqlQuery a -> SqlPersist m [SqlSelectRet r]
select query = do
  conn <- getConnection
  uncurry rawSql $ toRawSelectSql (fromDBName conn) query


-- | Get current database 'Connection'.
getConnection :: Monad m => SqlPersist m Connection
getConnection = SqlPersist R.ask


-- | Pretty prints a 'SqlQuery' into a SQL query.
toRawSelectSql :: SqlSelect a => Escape -> SqlQuery a -> (T.Text, [PersistValue])
toRawSelectSql esc query =
  let (ret, SideData fromClauses whereClauses) =
        flip S.evalSupply (idents ()) $
        W.runWriterT $
        unQ query

      (selectText, selectVars) = makeSelect esc ret
      (whereText,  whereVars)  = makeWhere  esc whereClauses

      text = TL.toStrict $
             TLB.toLazyText $
             mconcat
             [ "SELECT "
             , selectText
             , makeFrom esc fromClauses
             , whereText
             ]

  in (text, selectVars <> whereVars)


class RawSql (SqlSelectRet a) => SqlSelect a where
  type SqlSelectRet a :: *
  makeSelect :: Escape -> a -> (TLB.Builder, [PersistValue])

instance RawSql a => SqlSelect (SqlExpr a) where
  type SqlSelectRet (SqlExpr a) = a
  makeSelect _   (EEntity _) = ("??", mempty)
  makeSelect esc (ERaw f)    = first parens (f esc)

instance (SqlSelect a, SqlSelect b) => SqlSelect (a, b) where
  type SqlSelectRet (a, b) = (SqlSelectRet a, SqlSelectRet b)
  makeSelect esc (a, b) = uncommas' [makeSelect esc a, makeSelect esc b]
instance (SqlSelect a, SqlSelect b, SqlSelect c) => SqlSelect (a, b, c) where
  type SqlSelectRet (a, b, c) =
    ( SqlSelectRet a
    , SqlSelectRet b
    , SqlSelectRet c
    )
  makeSelect esc (a, b, c) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      , makeSelect esc c
      ]
instance ( SqlSelect a
         , SqlSelect b
         , SqlSelect c
         , SqlSelect d
         ) => SqlSelect (a, b, c, d) where
  type SqlSelectRet (a, b, c, d) =
    ( SqlSelectRet a
    , SqlSelectRet b
    , SqlSelectRet c
    , SqlSelectRet d
    )
  makeSelect esc (a, b, c, d) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      , makeSelect esc c
      , makeSelect esc d
      ]
instance ( SqlSelect a
         , SqlSelect b
         , SqlSelect c
         , SqlSelect d
         , SqlSelect e
         ) => SqlSelect (a, b, c, d, e) where
  type SqlSelectRet (a, b, c, d, e) =
    ( SqlSelectRet a
    , SqlSelectRet b
    , SqlSelectRet c
    , SqlSelectRet d
    , SqlSelectRet e
    )
  makeSelect esc (a, b, c, d, e) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      , makeSelect esc c
      , makeSelect esc d
      , makeSelect esc e
      ]
instance ( SqlSelect a
         , SqlSelect b
         , SqlSelect c
         , SqlSelect d
         , SqlSelect e
         , SqlSelect f
         ) => SqlSelect (a, b, c, d, e, f) where
  type SqlSelectRet (a, b, c, d, e, f) =
    ( SqlSelectRet a
    , SqlSelectRet b
    , SqlSelectRet c
    , SqlSelectRet d
    , SqlSelectRet e
    , SqlSelectRet f
    )
  makeSelect esc (a, b, c, d, e, f) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      , makeSelect esc c
      , makeSelect esc d
      , makeSelect esc e
      , makeSelect esc f
      ]
instance ( SqlSelect a
         , SqlSelect b
         , SqlSelect c
         , SqlSelect d
         , SqlSelect e
         , SqlSelect f
         , SqlSelect g
         ) => SqlSelect (a, b, c, d, e, f, g) where
  type SqlSelectRet (a, b, c, d, e, f, g) =
    ( SqlSelectRet a
    , SqlSelectRet b
    , SqlSelectRet c
    , SqlSelectRet d
    , SqlSelectRet e
    , SqlSelectRet f
    , SqlSelectRet g
    )
  makeSelect esc (a, b, c, d, e, f, g) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      , makeSelect esc c
      , makeSelect esc d
      , makeSelect esc e
      , makeSelect esc f
      , makeSelect esc g
      ]
instance ( SqlSelect a
         , SqlSelect b
         , SqlSelect c
         , SqlSelect d
         , SqlSelect e
         , SqlSelect f
         , SqlSelect g
         , SqlSelect h
         ) => SqlSelect (a, b, c, d, e, f, g, h) where
  type SqlSelectRet (a, b, c, d, e, f, g, h) =
    ( SqlSelectRet a
    , SqlSelectRet b
    , SqlSelectRet c
    , SqlSelectRet d
    , SqlSelectRet e
    , SqlSelectRet f
    , SqlSelectRet g
    , SqlSelectRet h
    )
  makeSelect esc (a, b, c, d, e, f, g, h) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      , makeSelect esc c
      , makeSelect esc d
      , makeSelect esc e
      , makeSelect esc f
      , makeSelect esc g
      , makeSelect esc h
      ]


uncommas :: [TLB.Builder] -> TLB.Builder
uncommas = mconcat . intersperse ", "

uncommas' :: Monoid a => [(TLB.Builder, a)] -> (TLB.Builder, a)
uncommas' = uncommas . map fst &&& mconcat . map snd


makeFrom :: Escape -> [FromClause] -> TLB.Builder
makeFrom esc = uncommas . map mk
  where
    mk (From (I i) def) = esc (entityDB def) <> (" AS " <> i)


makeWhere :: Escape -> WhereClause -> (TLB.Builder, [PersistValue])
makeWhere _   NoWhere          = mempty
makeWhere esc (Where (ERaw f)) = first ("\nWHERE " <>) (f esc)


parens :: TLB.Builder -> TLB.Builder
parens b = "(" <> (b <> "(")
