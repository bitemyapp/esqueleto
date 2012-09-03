{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, GADTs, OverloadedStrings #-}
module Database.Esqueleto.Internal.Sql
  ( SqlQuery
  , SqlExpr
  , select
  , selectSource
  , toRawSelectSql
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow (first)
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.List (intersperse)
import Data.Monoid (Monoid(..), (<>))
import Database.Persist.EntityDef
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Internal (Connection(escapeName))
import Database.Persist.GenericSql.Raw (withStmt)
import Database.Persist.Store
import qualified Control.Monad.Supply as S
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Writer as W
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
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


-- | A complete @WHERE@ clause.
data WhereClause = Where (SqlExpr (Single Bool))
                 | NoWhere

instance Monoid WhereClause where
  mempty = NoWhere
  NoWhere  `mappend` w        = w
  w        `mappend` NoWhere  = w
  Where e1 `mappend` Where e2 = Where (e1 &&. e2)


-- | Identifier used for tables.
newtype Ident = I TLB.Builder


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
selectSource :: ( SqlSelect a r
                , C.MonadResource m
                , MonadLogger m
                , MonadIO m )
             => SqlQuery a -> SqlPersist m (C.Source (SqlPersist m) r)
selectSource query = src
    where
      src = do
        conn <- getConnection
        return $ run conn C.$= massage

      run conn =
        uncurry withStmt $
        first (TL.toStrict . TLB.toLazyText) $
        toRawSelectSql (fromDBName conn) query

      massage = do
        mrow <- C.await
        case process <$> mrow of
          Just (Right r)  -> C.yield r >> massage
          Just (Left err) -> fail (T.unpack err)
          Nothing         -> return ()

      process = sqlSelectProcessRow


-- | Execute an Esqueleto's 'SqlQuery' inside @persistent@'s
-- 'SqlPersist' monad.
select :: ( SqlSelect a r
          , C.MonadResource m
          , MonadLogger m
          , MonadIO m )
       => SqlQuery a -> SqlPersist m [r]
select query = do
  src <- selectSource query
  src C.$$ CL.consume


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

      (_, selectText, selectVars) = sqlSelectCols esc ret
      (   whereText,  whereVars)  = makeWhere esc whereClauses

      text = mconcat
             [ "SELECT "
             , selectText
             , makeFrom esc fromClauses
             , whereText
             ]

  in (text, selectVars <> whereVars)


uncommas :: [TLB.Builder] -> TLB.Builder
uncommas = mconcat . intersperse ", "

uncommas' :: Monoid a => [(Int, TLB.Builder, a)] -> (Int, TLB.Builder, a)
uncommas' xs =
  let (as, bs, cs) = unzip3 xs
  in (sum as, uncommas bs, mconcat cs)


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
--
-- This looks very similar to @RawSql@, and it is!  However,
-- there are some crucial differences and ultimately they're
-- different classes.
class SqlSelect a r | a -> r, r -> a where
  -- | Creates the variable part of the @SELECT@ query and
  -- returns the list of 'PersistValue's that will be given to
  -- 'withStmt'.
  sqlSelectCols :: Escape -> a -> (Int, TLB.Builder, [PersistValue])

  -- | Transform a row of the result into the data type.
  sqlSelectProcessRow :: [PersistValue] -> Either T.Text r


instance PersistEntity a => SqlSelect (SqlExpr (Entity a)) (Entity a) where
  sqlSelectCols escape expr@(EEntity (I ident)) = ret
      where
        process ed = uncommas $
                     map ((name <>) . escape) $
                     (entityID ed:) $
                     map fieldDB $
                     entityFields ed
        -- 'name' is the biggest difference between 'RawSql' and
        -- 'SqlSelect'.  We automatically create names for tables
        -- (since it's not the user who's writing the FROM
        -- clause), while 'rawSql' assumes that it's just the
        -- name of the table (which doesn't allow self-joins, for
        -- example).
        name = ident <> "."
        ret = let ed = entityDef $ getEntityVal expr
              in (length (entityFields ed) + 1, process ed, mempty)
        getEntityVal :: SqlExpr (Entity a) -> a
        getEntityVal = error "Database.Esqueleto.SqlSelect.getEntityVal"
  sqlSelectProcessRow (idCol:ent) =
    Entity <$> fromPersistValue idCol
           <*> fromPersistValues ent
  sqlSelectProcessRow _ = Left "SqlSelect (Entity a): wrong number of columns."

instance PersistField a => SqlSelect (SqlExpr (Single a)) (Single a) where
  sqlSelectCols esc (ERaw f) = let (b, vals) = f esc
                               in (1, parens b, vals)
  sqlSelectProcessRow [pv] = Single <$> fromPersistValue pv
  sqlSelectProcessRow _    = Left "SqlSelect (Single a): wrong number of columns."

instance ( SqlSelect a ra
         , SqlSelect b rb
         ) => SqlSelect (a, b) (ra, rb) where
  sqlSelectCols esc (a, b) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      ]
  sqlSelectProcessRow =
    let x = getType processRow
        getType :: SqlSelect a r => (z -> Either y (r,x)) -> a
        getType = undefined

        (colCountFst, _, _) = sqlSelectCols undefined x
        processRow row =
            let (rowFst, rowSnd) = splitAt colCountFst row
            in (,) <$> sqlSelectProcessRow rowFst
                   <*> sqlSelectProcessRow rowSnd

    in colCountFst `seq` processRow
       -- Avoids recalculating 'colCountFst'.

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         ) => SqlSelect (a, b, c) (ra, rb, rc) where
  sqlSelectCols esc (a, b, c) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      ]
  sqlSelectProcessRow = fmap to3 . sqlSelectProcessRow

to3 :: ((a,b),c) -> (a,b,c)
to3 ((a,b),c) = (a,b,c)


instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         ) => SqlSelect (a, b, c, d) (ra, rb, rc, rd) where
  sqlSelectCols esc (a, b, c, d) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      ]
  sqlSelectProcessRow = fmap to4 . sqlSelectProcessRow

to4 :: ((a,b),(c,d)) -> (a,b,c,d)
to4 ((a,b),(c,d)) = (a,b,c,d)


instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         ) => SqlSelect (a, b, c, d, e) (ra, rb, rc, rd, re) where
  sqlSelectCols esc (a, b, c, d, e) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      ]
  sqlSelectProcessRow = fmap to5 . sqlSelectProcessRow

to5 :: ((a,b),(c,d),e) -> (a,b,c,d,e)
to5 ((a,b),(c,d),e) = (a,b,c,d,e)


instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         ) => SqlSelect (a, b, c, d, e, f) (ra, rb, rc, rd, re, rf) where
  sqlSelectCols esc (a, b, c, d, e, f) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      ]
  sqlSelectProcessRow = fmap to6 . sqlSelectProcessRow

to6 :: ((a,b),(c,d),(e,f)) -> (a,b,c,d,e,f)
to6 ((a,b),(c,d),(e,f)) = (a,b,c,d,e,f)


instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         ) => SqlSelect (a, b, c, d, e, f, g) (ra, rb, rc, rd, re, rf, rg) where
  sqlSelectCols esc (a, b, c, d, e, f, g) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      ]
  sqlSelectProcessRow = fmap to7 . sqlSelectProcessRow

to7 :: ((a,b),(c,d),(e,f),g) -> (a,b,c,d,e,f,g)
to7 ((a,b),(c,d),(e,f),g) = (a,b,c,d,e,f,g)


instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         ) => SqlSelect (a, b, c, d, e, f, g, h) (ra, rb, rc, rd, re, rf, rg, rh) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      ]
  sqlSelectProcessRow = fmap to8 . sqlSelectProcessRow

to8 :: ((a,b),(c,d),(e,f),(g,h)) -> (a,b,c,d,e,f,g,h)
to8 ((a,b),(c,d),(e,f),(g,h)) = (a,b,c,d,e,f,g,h)
