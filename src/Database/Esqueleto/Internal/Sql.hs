{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, GADTs, OverloadedStrings #-}
module Database.Esqueleto.Internal.Sql
  ( SqlQuery
  , SqlExpr
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

  sub query = ERaw $ \esc -> first parens (toRawSelectSql esc query)

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


-- | Execute an Esqueleto's 'SqlQuery' inside @persistent@'s
-- 'SqlPersist' monad.
select :: ( SqlSelect a r
          , RawSql r
          , MonadLogger m
          , MonadResourceBase m)
       => SqlQuery a -> SqlPersist m [r]
select query = do
  conn <- getConnection
  uncurry rawSql $
    first (TL.toStrict . TLB.toLazyText) $
    toRawSelectSql (fromDBName conn) query


-- | Get current database 'Connection'.
getConnection :: Monad m => SqlPersist m Connection
getConnection = SqlPersist R.ask


-- | Pretty prints a 'SqlQuery' into a SQL query.
toRawSelectSql :: SqlSelect a r => Escape -> SqlQuery a -> (TLB.Builder, [PersistValue])
toRawSelectSql esc query =
  let (ret, SideData fromClauses whereClauses) =
        flip S.evalSupply (idents ()) $
        W.runWriterT $
        unQ query

      (selectText, selectVars) = makeSelect esc ret
      (whereText,  whereVars)  = makeWhere  esc whereClauses

      text = mconcat
             [ "SELECT "
             , selectText
             , makeFrom esc fromClauses
             , whereText
             ]

  in (text, selectVars <> whereVars)


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


-- | Class for mapping results coming from 'SqlQuery' into actual
-- results.
class RawSql r => SqlSelect a r | a -> r, r -> a where
  makeSelect :: Escape -> a -> (TLB.Builder, [PersistValue])

instance PersistEntity a => SqlSelect (SqlExpr (Entity a)) (Entity a) where
  makeSelect _ (EEntity _) = ("??", mempty)

instance PersistField a => SqlSelect (SqlExpr (Single a)) (Single a) where
  makeSelect esc (ERaw f) = first parens (f esc)

instance ( SqlSelect a ra
         , SqlSelect b rb
         ) => SqlSelect (a, b) (ra, rb) where
  makeSelect esc (a, b) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      ]

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         ) => SqlSelect (a, b, c) (ra, rb, rc) where
  makeSelect esc (a, b, c) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      , makeSelect esc c
      ]

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         ) => SqlSelect (a, b, c, d) (ra, rb, rc, rd) where
  makeSelect esc (a, b, c, d) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      , makeSelect esc c
      , makeSelect esc d
      ]

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         ) => SqlSelect (a, b, c, d, e) (ra, rb, rc, rd, re) where
  makeSelect esc (a, b, c, d, e) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      , makeSelect esc c
      , makeSelect esc d
      , makeSelect esc e
      ]

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         ) => SqlSelect (a, b, c, d, e, f) (ra, rb, rc, rd, re, rf) where
  makeSelect esc (a, b, c, d, e, f) =
    uncommas'
      [ makeSelect esc a
      , makeSelect esc b
      , makeSelect esc c
      , makeSelect esc d
      , makeSelect esc e
      , makeSelect esc f
      ]

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         ) => SqlSelect (a, b, c, d, e, f, g) (ra, rb, rc, rd, re, rf, rg) where
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

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         ) => SqlSelect (a, b, c, d, e, f, g, h) (ra, rb, rc, rd, re, rf, rg, rh) where
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
