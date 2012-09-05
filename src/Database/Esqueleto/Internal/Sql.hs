{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, GADTs, OverloadedStrings #-}
module Database.Esqueleto.Internal.Sql
  ( SqlQuery
  , SqlExpr
  , select
  , selectSource
  , toRawSelectSql
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow ((***), first)
import Control.Exception (throw, throwIO)
import Control.Monad (ap, MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResourceBase)
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


-- | SQL backend for @esqueleto@ using 'SqlPersist'.
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
                         , sdOrderByClause :: ![OrderByClause]
                         }

instance Monoid SideData where
  mempty = SideData mempty mempty mempty
  SideData f w o `mappend` SideData f' w' o' =
    SideData (f <> f') (w <> w') (o <> o')


-- | A part of a @FROM@ clause.
data FromClause =
    FromStart Ident EntityDef
  | FromJoin FromClause JoinKind FromClause (Maybe (SqlExpr (Single Bool)))
  | OnClause (SqlExpr (Single Bool))


-- | Collect 'OnClause's on 'FromJoin's.  Returns the first
-- unmatched 'OnClause's data on error.  Returns a list without
-- 'OnClauses' on success.
collectOnClauses :: [FromClause] -> Either (SqlExpr (Single Bool)) [FromClause]
collectOnClauses = go []
  where
    go []  (f@(FromStart _ _):fs) = fmap (f:) (go [] fs) -- fast path
    go acc (OnClause expr    :fs) = findMatching acc expr >>= flip go fs
    go acc (f:fs)                 = go (f:acc) fs
    go acc []                     = return $ reverse acc

    findMatching (f : acc) expr =
      case tryMatch expr f of
        Just f' -> return (f' : acc)
        Nothing -> (f:) <$> findMatching acc expr
    findMatching [] expr = Left expr

    tryMatch expr (FromJoin l k r Nothing) =
      return (FromJoin l k r (Just expr))
    tryMatch expr (FromJoin l k r j@(Just _)) =
      ((\r' -> FromJoin l k r' j) <$> tryMatch expr r) `mplus`
      ((\l' -> FromJoin l' k r j) <$> tryMatch expr l)
    tryMatch _ _ = mzero


-- | A complete @WHERE@ clause.
data WhereClause = Where (SqlExpr (Single Bool))
                 | NoWhere

instance Monoid WhereClause where
  mempty = NoWhere
  NoWhere  `mappend` w        = w
  w        `mappend` NoWhere  = w
  Where e1 `mappend` Where e2 = Where (e1 &&. e2)


-- | A @ORDER BY@ clause.
type OrderByClause = SqlExpr OrderBy


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
  EEntity  :: Ident -> SqlExpr (Entity val)
  EMaybe   :: SqlExpr a -> SqlExpr (Maybe a)
  ERaw     :: (Escape -> (TLB.Builder, [PersistValue])) -> SqlExpr (Single a)
  EOrderBy :: OrderByType -> SqlExpr (Single a) -> SqlExpr OrderBy
  EPreprocessedFrom :: a -> FromClause -> SqlExpr (PreprocessedFrom a)

data OrderByType = ASC | DESC

type Escape = DBName -> TLB.Builder


instance Esqueleto SqlQuery SqlExpr SqlPersist where
  fromStart = Q $ do
    ident <- S.supply
    let ret   = EEntity ident
        from_ = FromStart ident $ entityDef (getVal ret)
    return (EPreprocessedFrom ret from_)

  fromStartMaybe = maybelize <$> fromStart
    where
      maybelize :: SqlExpr (PreprocessedFrom (SqlExpr (Entity a)))
                -> SqlExpr (PreprocessedFrom (SqlExpr (Maybe (Entity a))))
      maybelize (EPreprocessedFrom ret from_) = EPreprocessedFrom (EMaybe ret) from_
      maybelize _ = error "Esqueleto/Sql/fromStartMaybe: never here (see GHC #6124)"

  fromJoin (EPreprocessedFrom lhsRet lhsFrom)
           (EPreprocessedFrom rhsRet rhsFrom) = Q $ do
    let ret   = smartJoin lhsRet rhsRet
        from_ = FromJoin lhsFrom             -- LHS
                         (reifyJoinKind ret) -- JOIN
                         rhsFrom             -- RHS
                         Nothing             -- ON
    return (EPreprocessedFrom ret from_)
  fromJoin _ _ = error "Esqueleto/Sql/fromJoin: never here (see GHC #6124)"

  fromFinish (EPreprocessedFrom ret from_) = Q $ do
    W.tell mempty { sdFromClause = [from_] }
    return ret
  fromFinish _ = error "Esqueleto/Sql/fromFinish: never here (see GHC #6124)"

  where_ expr = Q $ W.tell mempty { sdWhereClause = Where expr }

  on expr = Q $ W.tell mempty { sdFromClause = [OnClause expr] }

  orderBy exprs = Q $ W.tell mempty { sdOrderByClause = exprs }
  asc  = EOrderBy ASC
  desc = EOrderBy DESC

  sub query = ERaw $ \esc -> first parens (toRawSelectSql esc query)

  EEntity (I ident) ^. field = ERaw $ \esc -> (ident <> ("." <> name esc field), [])
      where name esc = esc . fieldDB . persistFieldDef
  _ ^. _ = error "Esqueleto/Sql/(^.): never here (see GHC #6124)"

  EMaybe r ?. field = maybelize (r ^. field)
    where
      maybelize :: SqlExpr (Single a) -> SqlExpr (Single (Maybe a))
      maybelize (ERaw f) = ERaw f
      maybelize _        = error "Esqueleto/Sql/(?.): never here 1 (see GHC #6124)"
  _ ?. _ = error "Esqueleto/Sql/(?.): never here 2 (see GHC #6124)"

  val = ERaw . const . (,) "?" . return . toPersistValue

  isNothing (ERaw f) = ERaw $ first ((<> " IS NULL") . parens) . f
  isNothing _        = error "Esqueleto/Sql/isNothing: never here (see GHC #6124)"
  just (ERaw f) = ERaw f
  just _        = error "Esqueleto/Sql/just: never here (see GHC #6124)"
  nothing = ERaw $ \_ -> ("NULL", mempty)

  not_ (ERaw f) = ERaw $ \esc -> let (b, vals) = f esc
                                 in ("NOT " <> parens b, vals)
  not_ _ = error "Esqueleto/Sql/not_: never here (see GHC #6124)"

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


getVal :: SqlExpr (Entity val) -> val
getVal = error "SqlQuery/getVal: never here"

fromDBName :: Connection -> DBName -> TLB.Builder
fromDBName conn = TLB.fromText . escapeName conn

binop :: TLB.Builder -> SqlExpr (Single a) -> SqlExpr (Single b) -> SqlExpr (Single c)
binop op (ERaw f1) (ERaw f2) = ERaw f
  where
    f esc = let (b1, vals1) = f1 esc
                (b2, vals2) = f2 esc
            in ( parens b1 <> op <> parens b2
               , vals1 <> vals2 )
binop _ _ _ = error "Esqueleto/Sql/binop: never here (see GHC #6124)"


-- | Execute an Esqueleto's 'SqlQuery' inside @persistent@'s
-- 'SqlPersist' monad.
selectSource :: ( SqlSelect a r
                , MonadLogger m
                , MonadResourceBase m )
             => SqlQuery a -> SqlPersist m (C.Source (C.ResourceT (SqlPersist m)) r)
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
          Just (Left err) -> liftIO $ throwIO $ PersistMarshalError err
          Nothing         -> return ()

      process = sqlSelectProcessRow


-- | Execute an Esqueleto's 'SqlQuery' inside @persistent@'s
-- 'SqlPersist' monad.
select :: ( SqlSelect a r
          , MonadLogger m
          , MonadResourceBase m )
       => SqlQuery a -> SqlPersist m [r]
select query = do
  src <- selectSource query
  C.runResourceT $ src C.$$ CL.consume


-- | Get current database 'Connection'.
getConnection :: Monad m => SqlPersist m Connection
getConnection = SqlPersist R.ask


-- | Pretty prints a 'SqlQuery' into a SQL query.
toRawSelectSql :: SqlSelect a r => Escape -> SqlQuery a -> (TLB.Builder, [PersistValue])
toRawSelectSql esc query =
  let (ret, SideData fromClauses whereClauses orderByClauses) =
        flip S.evalSupply (idents ()) $
        W.runWriterT $
        unQ query
  in mconcat
      [ makeSelect  esc ret
      , makeFrom    esc fromClauses
      , makeWhere   esc whereClauses
      , makeOrderBy esc orderByClauses
      ]


uncommas :: [TLB.Builder] -> TLB.Builder
uncommas = mconcat . intersperse ", "

uncommas' :: Monoid a => [(TLB.Builder, a)] -> (TLB.Builder, a)
uncommas' = (uncommas *** mconcat) . unzip


makeSelect :: SqlSelect a r => Escape -> a -> (TLB.Builder, [PersistValue])
makeSelect esc ret = first ("SELECT " <>) (sqlSelectCols esc ret)


makeFrom :: Escape -> [FromClause] -> (TLB.Builder, [PersistValue])
makeFrom _   [] = mempty
makeFrom esc fs = ret
  where
    ret = case collectOnClauses fs of
            Left expr -> throw $ mkExc expr
            Right fs' -> first ("\nFROM " <>) $ uncommas' (map mk fs')

    mk (FromStart (I i) def) = base i def
    mk (FromJoin lhs kind rhs monClause) =
      mconcat [ mk lhs
              , (fromKind kind, mempty)
              , mk rhs
              , maybe mempty makeOnClause monClause ]
    mk (OnClause _) = error "Esqueleto/Sql/makeFrom: never here (is collectOnClauses working?)"

    base i def = (esc (entityDB def) <> (" AS " <> i), mempty)

    fromKind InnerJoinKind      = " INNER JOIN "
    fromKind CrossJoinKind      = " CROSS JOIN "
    fromKind LeftOuterJoinKind  = " LEFT OUTER JOIN "
    fromKind RightOuterJoinKind = " RIGHT OUTER JOIN "
    fromKind FullOuterJoinKind  = " FULL OUTER JOIN "

    makeOnClause (ERaw f) = first (" ON " <>) (f esc)
    makeOnClause _ = error "Esqueleto/Sql/makeFrom/makeOnClause: never here (see GHC #6124)"

    mkExc (ERaw f) =
      OnClauseWithoutMatchingJoinException $
      TL.unpack $ TLB.toLazyText $ fst (f esc)
    mkExc _ = OnClauseWithoutMatchingJoinException "???"


makeWhere :: Escape -> WhereClause -> (TLB.Builder, [PersistValue])
makeWhere _   NoWhere          = mempty
makeWhere esc (Where (ERaw f)) = first ("\nWHERE " <>) (f esc)
makeWhere _ _ = error "Esqueleto/Sql/makeWhere: never here (see GHC #6124)"


makeOrderBy :: Escape -> [OrderByClause] -> (TLB.Builder, [PersistValue])
makeOrderBy _   [] = mempty
makeOrderBy esc os = first ("\nORDER BY " <>) $ uncommas' (map mk os)
  where
    mk (EOrderBy t (ERaw f)) = first (<> orderByType t) (f esc)
    mk _ = error "Esqueleto/Sql/makeOrderBy: never here (see GHC #6124)"
    orderByType ASC  = " ASC"
    orderByType DESC = " DESC"


parens :: TLB.Builder -> TLB.Builder
parens b = "(" <> (b <> ")")


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
  sqlSelectCols :: Escape -> a -> (TLB.Builder, [PersistValue])

  -- | Number of columns that will be consumed.  Must be
  -- non-strict on the argument.
  sqlSelectColCount :: a -> Int

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
              in (process ed, mempty)
  sqlSelectCols _ _ = error "Esqueleto/Sql/sqlSelectCols[Entity]: never here (see GHC #6124)"
  sqlSelectColCount = (+1) . length . entityFields . entityDef . getEntityVal
  sqlSelectProcessRow (idCol:ent) =
    Entity <$> fromPersistValue idCol
           <*> fromPersistValues ent
  sqlSelectProcessRow _ = Left "SqlSelect (Entity a): wrong number of columns."

getEntityVal :: SqlExpr (Entity a) -> a
getEntityVal = error "Esqueleto/Sql/getEntityVal"

instance PersistEntity a => SqlSelect (SqlExpr (Maybe (Entity a))) (Maybe (Entity a)) where
  sqlSelectCols escape (EMaybe ent) = sqlSelectCols escape ent
  sqlSelectCols _ _ = error "Esqueleto/Sql/sqlSelectCols[Maybe Entity]: never here (see GHC #6124)"
  sqlSelectColCount = sqlSelectColCount . fromEMaybe
    where
      fromEMaybe :: SqlExpr (Maybe e) -> SqlExpr e
      fromEMaybe = error "Esqueleto/Sql/sqlSelectColCount[Maybe Entity]/fromEMaybe"
  sqlSelectProcessRow cols
    | all (== PersistNull) cols = return Nothing
    | otherwise                 = Just <$> sqlSelectProcessRow cols

instance PersistField a => SqlSelect (SqlExpr (Single a)) (Single a) where
  sqlSelectCols esc (ERaw f) = let (b, vals) = f esc
                               in (parens b, vals)
  sqlSelectCols _ _ = error "Esqueleto/Sql/sqlSelectCols[Single]: never here (see GHC #6124)"
  sqlSelectColCount = const 1
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
  sqlSelectColCount ~(a,b) = sqlSelectColCount a + sqlSelectColCount b
  sqlSelectProcessRow =
    let x = getType processRow
        getType :: SqlSelect a r => (z -> Either y (r,x)) -> a
        getType = error "Esqueleto/SqlSelect[(a,b)]/sqlSelectProcessRow/getType"

        colCountFst = sqlSelectColCount x

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
  sqlSelectColCount   = sqlSelectColCount . from3
  sqlSelectProcessRow = fmap to3 . sqlSelectProcessRow

from3 :: (a,b,c) -> ((a,b),c)
from3 (a,b,c) = ((a,b),c)

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
  sqlSelectColCount   = sqlSelectColCount . from4
  sqlSelectProcessRow = fmap to4 . sqlSelectProcessRow

from4 :: (a,b,c,d) -> ((a,b),(c,d))
from4 (a,b,c,d) = ((a,b),(c,d))

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
  sqlSelectColCount   = sqlSelectColCount . from5
  sqlSelectProcessRow = fmap to5 . sqlSelectProcessRow

from5 :: (a,b,c,d,e) -> ((a,b),(c,d),e)
from5 (a,b,c,d,e) = ((a,b),(c,d),e)

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
  sqlSelectColCount   = sqlSelectColCount . from6
  sqlSelectProcessRow = fmap to6 . sqlSelectProcessRow

from6 :: (a,b,c,d,e,f) -> ((a,b),(c,d),(e,f))
from6 (a,b,c,d,e,f) = ((a,b),(c,d),(e,f))

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
  sqlSelectColCount   = sqlSelectColCount . from7
  sqlSelectProcessRow = fmap to7 . sqlSelectProcessRow

from7 :: (a,b,c,d,e,f,g) -> ((a,b),(c,d),(e,f),g)
from7 (a,b,c,d,e,f,g) = ((a,b),(c,d),(e,f),g)

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
  sqlSelectColCount   = sqlSelectColCount . from8
  sqlSelectProcessRow = fmap to8 . sqlSelectProcessRow

from8 :: (a,b,c,d,e,f,g,h) -> ((a,b),(c,d),(e,f),(g,h))
from8 (a,b,c,d,e,f,g,h) = ((a,b),(c,d),(e,f),(g,h))

to8 :: ((a,b),(c,d),(e,f),(g,h)) -> (a,b,c,d,e,f,g,h)
to8 ((a,b),(c,d),(e,f),(g,h)) = (a,b,c,d,e,f,g,h)
