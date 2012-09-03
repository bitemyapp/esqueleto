{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, GADTs, OverloadedStrings #-}
module Database.Esqueleto.Internal.Sql
  ( SqlQuery
  , select
  , toRawSelectSql
  ) where

import Control.Applicative (Applicative(..), (<$>))
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
  ERaw    :: (Connection -> TLB.Builder) -> [PersistValue] -> SqlExpr (Single a)

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

  EEntity (I ident) ^. field = ERaw (\conn -> ident <> ("." <> name conn field)) []
      where name conn = fromDBName conn . fieldDB . persistFieldDef

  val = ERaw (const "?") . return . toPersistValue

  not_ (ERaw b vals) = ERaw (\conn -> "NOT " <> parens (b conn)) vals

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
binop op (ERaw b1 vals1) (ERaw b2 vals2) = ERaw b (vals1 <> vals2)
  where
    b conn = parens (b1 conn) <> op <> parens (b2 conn)


-- | TODO
select :: ( SqlSelect a
          , RawSql (SqlSelectRet r)
          , MonadLogger m
          , MonadResourceBase m)
       => SqlQuery a -> SqlPersist m [SqlSelectRet r]
select query = do
  conn <- getConnection
  uncurry rawSql $ toRawSelectSql conn query


-- | Get current database 'Connection'.
getConnection :: Monad m => SqlPersist m Connection
getConnection = SqlPersist R.ask


-- | Pretty prints a 'SqlQuery' into a SQL query.
toRawSelectSql :: SqlSelect a => Connection -> SqlQuery a -> (T.Text, [PersistValue])
toRawSelectSql conn query =
  let (ret, SideData fromClauses whereClauses) =
        flip S.evalSupply (idents ()) $
        W.runWriterT $
        unQ query

      (selectText, selectVars) = makeSelect conn ret
      (whereText,  whereVars)  = makeWhere  conn whereClauses

      text = TL.toStrict $
             TLB.toLazyText $
             mconcat
             [ "SELECT "
             , selectText
             , makeFrom conn fromClauses
             , whereText
             ]

  in (text, selectVars <> whereVars)


class RawSql (SqlSelectRet a) => SqlSelect a where
  type SqlSelectRet a :: *
  makeSelect :: Connection -> a -> (TLB.Builder, [PersistValue])

instance RawSql a => SqlSelect (SqlExpr a) where
  type SqlSelectRet (SqlExpr a) = a
  makeSelect _    (EEntity _)   = ("??", mempty)
  makeSelect conn (ERaw b vals) = (parens (b conn), vals)

instance (SqlSelect a, SqlSelect b) => SqlSelect (a, b) where
  type SqlSelectRet (a, b) = (SqlSelectRet a, SqlSelectRet b)
  makeSelect conn (a, b) = makeSelect conn a <> makeSelect conn b
instance (SqlSelect a, SqlSelect b, SqlSelect c) => SqlSelect (a, b, c) where
  type SqlSelectRet (a, b, c) =
    ( SqlSelectRet a
    , SqlSelectRet b
    , SqlSelectRet c
    )
  makeSelect conn (a, b, c) =
    mconcat
      [ makeSelect conn a
      , makeSelect conn b
      , makeSelect conn c
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
  makeSelect conn (a, b, c, d) =
    mconcat
      [ makeSelect conn a
      , makeSelect conn b
      , makeSelect conn c
      , makeSelect conn d
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
  makeSelect conn (a, b, c, d, e) =
    mconcat
      [ makeSelect conn a
      , makeSelect conn b
      , makeSelect conn c
      , makeSelect conn d
      , makeSelect conn e
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
  makeSelect conn (a, b, c, d, e, f) =
    mconcat
      [ makeSelect conn a
      , makeSelect conn b
      , makeSelect conn c
      , makeSelect conn d
      , makeSelect conn e
      , makeSelect conn f
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
  makeSelect conn (a, b, c, d, e, f, g) =
    mconcat
      [ makeSelect conn a
      , makeSelect conn b
      , makeSelect conn c
      , makeSelect conn d
      , makeSelect conn e
      , makeSelect conn f
      , makeSelect conn g
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
  makeSelect conn (a, b, c, d, e, f, g, h) =
    mconcat
      [ makeSelect conn a
      , makeSelect conn b
      , makeSelect conn c
      , makeSelect conn d
      , makeSelect conn e
      , makeSelect conn f
      , makeSelect conn g
      , makeSelect conn h
      ]


makeFrom :: Connection -> [FromClause] -> TLB.Builder
makeFrom conn = mconcat . intersperse ", " . map mk
  where
    mk (From (I i) def) = fromDBName conn (entityDB def) <> (" AS " <> i)


makeWhere :: Connection -> WhereClause -> (TLB.Builder, [PersistValue])
makeWhere _    NoWhere               = mempty
makeWhere conn (Where (ERaw b vals)) = ("\nWHERE " <> b conn, vals)


parens :: TLB.Builder -> TLB.Builder
parens b = "(" <> (b <> "(")
