{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , MultiParamTypeClasses
           , OverloadedStrings
           , UndecidableInstances
 #-}
-- | This is an internal module, anything exported by this module
-- may change without a major version bump.  Please use only
-- "Database.Esqueleto" if possible.
module Database.Esqueleto.Internal.Sql
  ( -- * The pretty face
    SqlQuery
  , SqlExpr
  , SqlEntity
  , select
  , selectSource
  , selectDistinct
  , selectDistinctSource
  , delete
  , deleteCount
  , update
  , updateCount
    -- * The guts
  , unsafeSqlBinOp
  , unsafeSqlValue
  , unsafeSqlFunction
  , UnsafeSqlFunctionArgument
  , rawSelectSource
  , runSource
  , rawEsqueleto
  , toRawSql
  , Mode(..)
  , SqlSelect
  , veryUnsafeCoerceSqlExprValue
  , insertSelectDistinct
  , insertSelect
  , (<#)
  , (<&>)
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Arrow ((***), first)
import Control.Exception (throw, throwIO)
import Control.Monad ((>=>), ap, void, MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Monoid (Monoid(..), (<>))
import Data.Proxy (Proxy(..))
import Database.Esqueleto.Internal.PersistentImport
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Writer as W
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLBI

import Database.Esqueleto.Internal.Language


-- | SQL backend for @esqueleto@ using 'SqlPersistT'.
newtype SqlQuery a =
  Q { unQ :: W.WriterT SideData (S.State IdentState) a }

instance Functor SqlQuery where
  fmap f = Q . fmap f . unQ

instance Monad SqlQuery where
  return  = Q . return
  m >>= f = Q (unQ m >>= unQ . f)

instance Applicative SqlQuery where
  pure  = return
  (<*>) = ap


-- | Constraint synonym for @persistent@ entities whose backend
-- is 'SqlPersistT'.
type SqlEntity ent = (PersistEntity ent, PersistEntityBackend ent ~ SqlBackend)


----------------------------------------------------------------------


-- | Side data written by 'SqlQuery'.
data SideData = SideData { sdFromClause    :: ![FromClause]
                         , sdSetClause     :: ![SetClause]
                         , sdWhereClause   :: !WhereClause
                         , sdGroupByClause :: !GroupByClause
                         , sdHavingClause  :: !HavingClause
                         , sdOrderByClause :: ![OrderByClause]
                         , sdLimitClause   :: !LimitClause
                         }

instance Monoid SideData where
  mempty = SideData mempty mempty mempty mempty mempty mempty mempty
  SideData f s w g h o l `mappend` SideData f' s' w' g' h' o' l' =
    SideData (f <> f') (s <> s') (w <> w') (g <> g') (h <> h') (o <> o') (l <> l')


-- | A part of a @FROM@ clause.
data FromClause =
    FromStart Ident (EntityDef SqlType)
  | FromJoin FromClause JoinKind FromClause (Maybe (SqlExpr (Value Bool)))
  | OnClause (SqlExpr (Value Bool))


-- | A part of a @SET@ clause.
newtype SetClause = SetClause (SqlExpr (Value ()))


-- | Collect 'OnClause's on 'FromJoin's.  Returns the first
-- unmatched 'OnClause's data on error.  Returns a list without
-- 'OnClauses' on success.
collectOnClauses :: [FromClause] -> Either (SqlExpr (Value Bool)) [FromClause]
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

    tryMatch expr (FromJoin l k r onClause) =
      matchR `mplus` matchC `mplus` matchL -- right to left
        where
          matchR = (\r' -> FromJoin l k r' onClause) <$> tryMatch expr r
          matchL = (\l' -> FromJoin l' k r onClause) <$> tryMatch expr l
          matchC = case onClause of
                     Nothing -> return (FromJoin l k r (Just expr))
                     Just _  -> mzero
    tryMatch _ _ = mzero


-- | A complete @WHERE@ clause.
data WhereClause = Where (SqlExpr (Value Bool))
                 | NoWhere

instance Monoid WhereClause where
  mempty = NoWhere
  NoWhere  `mappend` w        = w
  w        `mappend` NoWhere  = w
  Where e1 `mappend` Where e2 = Where (e1 &&. e2)


-- | A @GROUP BY@ clause.
newtype GroupByClause = GroupBy [SomeValue SqlExpr]

instance Monoid GroupByClause where
  mempty = GroupBy []
  GroupBy fs `mappend` GroupBy fs' = GroupBy (fs <> fs')

-- | A @HAVING@ cause.
type HavingClause = WhereClause

-- | A @ORDER BY@ clause.
type OrderByClause = SqlExpr OrderBy


-- | A @LIMIT@ clause.
data LimitClause = Limit (Maybe Int64) (Maybe Int64)

instance Monoid LimitClause where
  mempty = Limit mzero mzero
  Limit l1 o1 `mappend` Limit l2 o2 =
    Limit (l2 `mplus` l1) (o2 `mplus` o1)
    -- More than one 'limit' or 'offset' is issued, we want to
    -- keep the latest one.  That's why we use mplus with
    -- "reversed" arguments.


----------------------------------------------------------------------


-- | Identifier used for table names.
newtype Ident = I T.Text


-- | List of identifiers already in use and supply of temporary
-- identifiers.
newtype IdentState = IdentState { inUse :: HS.HashSet T.Text }

initialIdentState :: IdentState
initialIdentState = IdentState mempty


-- | Create a fresh 'Ident'.  If possible, use the given
-- 'DBName'.
newIdentFor :: DBName -> SqlQuery Ident
newIdentFor = Q . lift . try . unDBName
  where
    try orig = do
      s <- S.get
      let go (t:ts) | t `HS.member` inUse s = go ts
                    | otherwise             = use t
          go [] = error "Esqueleto/Sql/newIdentFor: never here"
      go (possibilities orig)

    possibilities t = t : map addNum [2..]
      where
        addNum :: Int -> T.Text
        addNum = T.append t . T.pack . show

    use t = do
      S.modify (\s -> s { inUse = HS.insert t (inUse s) })
      return (I t)


-- | Use an identifier.
useIdent :: Connection -> Ident -> TLB.Builder
useIdent conn (I ident) = fromDBName conn $ DBName ident


----------------------------------------------------------------------

type Insertion = Proxy

-- | An expression on the SQL backend.
data SqlExpr a where
  EInsert  :: Proxy a -> (Connection -> (TLB.Builder, [PersistValue])) -> SqlExpr (Insertion a)
  EEntity  :: Ident -> SqlExpr (Entity val)
  EMaybe   :: SqlExpr a -> SqlExpr (Maybe a)
  ERaw     :: NeedParens -> (Connection -> (TLB.Builder, [PersistValue])) -> SqlExpr (Value a)
  EList    :: SqlExpr (Value a) -> SqlExpr (ValueList a)
  EEmptyList :: SqlExpr (ValueList a)
  EOrderBy :: OrderByType -> SqlExpr (Value a) -> SqlExpr OrderBy
  ESet     :: (SqlExpr (Entity val) -> SqlExpr (Value ())) -> SqlExpr (Update val)
  EPreprocessedFrom :: a -> FromClause -> SqlExpr (PreprocessedFrom a)

data NeedParens = Parens | Never

parensM :: NeedParens -> TLB.Builder -> TLB.Builder
parensM Never  = id
parensM Parens = parens

data OrderByType = ASC | DESC


instance Esqueleto SqlQuery SqlExpr SqlBackend where
  fromStart = x
    where
      x = do
        let ed = entityDef (getVal x)
        ident <- newIdentFor (entityDB ed)
        let ret   = EEntity ident
            from_ = FromStart ident ed
        return (EPreprocessedFrom ret from_)
      getVal :: SqlQuery (SqlExpr (PreprocessedFrom (SqlExpr (Entity a)))) -> Proxy a
      getVal = const Proxy

  fromStartMaybe = maybelize <$> fromStart
    where
      maybelize :: SqlExpr (PreprocessedFrom (SqlExpr (Entity a)))
                -> SqlExpr (PreprocessedFrom (SqlExpr (Maybe (Entity a))))
      maybelize (EPreprocessedFrom ret from_) = EPreprocessedFrom (EMaybe ret) from_

  fromJoin (EPreprocessedFrom lhsRet lhsFrom)
           (EPreprocessedFrom rhsRet rhsFrom) = Q $ do
    let ret   = smartJoin lhsRet rhsRet
        from_ = FromJoin lhsFrom             -- LHS
                         (reifyJoinKind ret) -- JOIN
                         rhsFrom             -- RHS
                         Nothing             -- ON
    return (EPreprocessedFrom ret from_)

  fromFinish (EPreprocessedFrom ret from_) = Q $ do
    W.tell mempty { sdFromClause = [from_] }
    return ret

  where_ expr = Q $ W.tell mempty { sdWhereClause = Where expr }

  on expr = Q $ W.tell mempty { sdFromClause = [OnClause expr] }

  groupBy expr = Q $ W.tell mempty { sdGroupByClause = GroupBy $ toSomeValues expr }

  having expr = Q $ W.tell mempty { sdHavingClause = Where expr }

  orderBy exprs = Q $ W.tell mempty { sdOrderByClause = exprs }
  asc  = EOrderBy ASC
  desc = EOrderBy DESC

  limit  n = Q $ W.tell mempty { sdLimitClause = Limit (Just n) Nothing  }
  offset n = Q $ W.tell mempty { sdLimitClause = Limit Nothing  (Just n) }

  sub_select         = sub SELECT
  sub_selectDistinct = sub SELECT_DISTINCT

  EEntity ident ^. field =
    ERaw Never $ \conn -> (useIdent conn ident <> ("." <> fieldName conn field), [])

  EMaybe r ?. field = maybelize (r ^. field)
    where
      maybelize :: SqlExpr (Value a) -> SqlExpr (Value (Maybe a))
      maybelize (ERaw p f) = ERaw p f

  val = ERaw Never . const . (,) "?" . return . toPersistValue

  isNothing (ERaw p f) = ERaw Parens $ first ((<> " IS NULL") . parensM p) . f
  just (ERaw p f) = ERaw p f
  nothing   = unsafeSqlValue "NULL"
  joinV (ERaw p f) = ERaw p f
  countRows = unsafeSqlValue "COUNT(*)"
  count (ERaw _ f) = ERaw Never $ \conn -> let (b, vals) = f conn
                                           in ("COUNT" <> parens b, vals)

  not_ (ERaw p f) = ERaw Never $ \conn -> let (b, vals) = f conn
                                          in ("NOT " <> parensM p b, vals)

  (==.) = unsafeSqlBinOp " = "
  (>=.) = unsafeSqlBinOp " >= "
  (>.)  = unsafeSqlBinOp " > "
  (<=.) = unsafeSqlBinOp " <= "
  (<.)  = unsafeSqlBinOp " < "
  (!=.) = unsafeSqlBinOp " != "
  (&&.) = unsafeSqlBinOp " AND "
  (||.) = unsafeSqlBinOp " OR "
  (+.)  = unsafeSqlBinOp " + "
  (-.)  = unsafeSqlBinOp " - "
  (/.)  = unsafeSqlBinOp " / "
  (*.)  = unsafeSqlBinOp " * "

  random_  = unsafeSqlValue "RANDOM()"
  sum_     = unsafeSqlFunction "SUM"
  round_   = unsafeSqlFunction "ROUND"
  ceiling_ = unsafeSqlFunction "CEILING"
  floor_   = unsafeSqlFunction "FLOOR"
  avg_     = unsafeSqlFunction "AVG"
  min_     = unsafeSqlFunction "MIN"
  max_     = unsafeSqlFunction "MAX"

  like    = unsafeSqlBinOp    " LIKE "
  (%)     = unsafeSqlValue    "'%'"
  concat_ = unsafeSqlFunction "CONCAT"
  (++.)   = unsafeSqlBinOp    " || "

  subList_select         = EList . sub_select
  subList_selectDistinct = EList . sub_selectDistinct

  valList []   = EEmptyList
  valList vals = EList $ ERaw Parens $ const ( uncommas ("?" <$ vals)
                                             , map toPersistValue vals )

  v `in_`   e = ifNotEmptyList e False $ unsafeSqlBinOp     " IN " v (veryUnsafeCoerceSqlExprValueList e)
  v `notIn` e = ifNotEmptyList e True  $ unsafeSqlBinOp " NOT IN " v (veryUnsafeCoerceSqlExprValueList e)

  exists    = unsafeSqlFunction     "EXISTS " . existsHelper
  notExists = unsafeSqlFunction "NOT EXISTS " . existsHelper

  set ent upds = Q $ W.tell mempty { sdSetClause = map apply upds }
    where
      apply (ESet f) = SetClause (f ent)

  field  =. expr = setAux field (const expr)
  field +=. expr = setAux field (\ent -> ent ^. field +. expr)
  field -=. expr = setAux field (\ent -> ent ^. field -. expr)
  field *=. expr = setAux field (\ent -> ent ^. field *. expr)
  field /=. expr = setAux field (\ent -> ent ^. field /. expr)


instance ToSomeValues SqlExpr (SqlExpr (Value a)) where
  toSomeValues a = [SomeValue a]

fieldName :: (PersistEntity val, PersistField typ)
          => Connection -> EntityField val typ -> TLB.Builder
fieldName conn = fromDBName conn . fieldDB . persistFieldDef

setAux :: (PersistEntity val, PersistField typ)
       => EntityField val typ
       -> (SqlExpr (Entity val) -> SqlExpr (Value typ))
       -> SqlExpr (Update val)
setAux field mkVal = ESet $ \ent -> unsafeSqlBinOp " = " name (mkVal ent)
  where name = ERaw Never $ \conn -> (fieldName conn field, mempty)

sub :: PersistField a => Mode -> SqlQuery (SqlExpr (Value a)) -> SqlExpr (Value a)
sub mode query = ERaw Parens $ \conn -> toRawSql mode pureQuery conn query

fromDBName :: Connection -> DBName -> TLB.Builder
fromDBName conn = TLB.fromText . connEscapeName conn

existsHelper :: SqlQuery () -> SqlExpr (Value a)
existsHelper =
  ERaw Parens .
  flip (toRawSql SELECT pureQuery) .
  (>> return (val True :: SqlExpr (Value Bool)))

ifNotEmptyList :: SqlExpr (ValueList a) -> Bool -> SqlExpr (Value Bool) -> SqlExpr (Value Bool)
ifNotEmptyList EEmptyList b _ = val b
ifNotEmptyList (EList _)  _ x = x


----------------------------------------------------------------------


-- | (Internal) Create a custom binary operator.  You /should/
-- /not/ use this function directly since its type is very
-- general, you should always use it with an explicit type
-- signature.  For example:
--
-- @
-- (==.) :: SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value Bool)
-- (==.) = unsafeSqlBinOp " = "
-- @
--
-- In the example above, we constraint the arguments to be of the
-- same type and constraint the result to be a boolean value.
unsafeSqlBinOp :: TLB.Builder -> SqlExpr (Value a) -> SqlExpr (Value b) -> SqlExpr (Value c)
unsafeSqlBinOp op (ERaw p1 f1) (ERaw p2 f2) = ERaw Parens f
  where
    f conn = let (b1, vals1) = f1 conn
                 (b2, vals2) = f2 conn
             in ( parensM p1 b1 <> op <> parensM p2 b2
                , vals1 <> vals2 )
{-# INLINE unsafeSqlBinOp #-}


-- | (Internal) A raw SQL value.  The same warning from
-- 'unsafeSqlBinOp' applies to this function as well.
unsafeSqlValue :: TLB.Builder -> SqlExpr (Value a)
unsafeSqlValue v = ERaw Never $ \_ -> (v, mempty)
{-# INLINE unsafeSqlValue #-}


-- | (Internal) A raw SQL function.  Once again, the same warning
-- from 'unsafeSqlBinOp' applies to this function as well.
unsafeSqlFunction :: UnsafeSqlFunctionArgument a =>
                     TLB.Builder -> a -> SqlExpr (Value b)
unsafeSqlFunction name arg =
  ERaw Never $ \conn ->
    let (argsTLB, argsVals) =
          uncommas' $ map (\(ERaw _ f) -> f conn) $ toArgList arg
    in (name <> parens argsTLB, argsVals)

class UnsafeSqlFunctionArgument a where
  toArgList :: a -> [SqlExpr (Value ())]
instance (a ~ Value b) => UnsafeSqlFunctionArgument (SqlExpr a) where
  toArgList = (:[]) . veryUnsafeCoerceSqlExprValue
instance UnsafeSqlFunctionArgument a =>
         UnsafeSqlFunctionArgument [a] where
  toArgList = concatMap toArgList
instance ( UnsafeSqlFunctionArgument a
         , UnsafeSqlFunctionArgument b
         ) => UnsafeSqlFunctionArgument (a, b) where
  toArgList (a, b) = toArgList a ++ toArgList b
instance ( UnsafeSqlFunctionArgument a
         , UnsafeSqlFunctionArgument b
         , UnsafeSqlFunctionArgument c
         ) => UnsafeSqlFunctionArgument (a, b, c) where
  toArgList = toArgList . from3
instance ( UnsafeSqlFunctionArgument a
         , UnsafeSqlFunctionArgument b
         , UnsafeSqlFunctionArgument c
         , UnsafeSqlFunctionArgument d
         ) => UnsafeSqlFunctionArgument (a, b, c, d) where
  toArgList = toArgList . from4


-- | (Internal) Coerce a value's type from 'SqlExpr (Value a)' to
-- 'SqlExpr (Value b)'.  You should /not/ use this function
-- unless you know what you're doing!
veryUnsafeCoerceSqlExprValue :: SqlExpr (Value a) -> SqlExpr (Value b)
veryUnsafeCoerceSqlExprValue (ERaw p f) = ERaw p f


-- | (Internal) Coerce a value's type from 'SqlExpr (ValueList
-- a)' to 'SqlExpr (Value a)'.  Does not work with empty lists.
veryUnsafeCoerceSqlExprValueList :: SqlExpr (ValueList a) -> SqlExpr (Value a)
veryUnsafeCoerceSqlExprValueList (EList v)  = v
veryUnsafeCoerceSqlExprValueList EEmptyList =
  error "veryUnsafeCoerceSqlExprValueList: empty list."


----------------------------------------------------------------------


-- | (Internal) Execute an @esqueleto@ @SELECT@ 'SqlQuery' inside
-- @persistent@'s 'SqlPersistT' monad.
rawSelectSource :: ( SqlSelect a r
                   , MonadLogger m
                   , MonadResourceBase m )
                 => Mode
                 -> SqlQuery a
                 -> SqlPersistT m (C.Source (C.ResourceT (SqlPersistT m)) r)
rawSelectSource mode query = src
    where
      src = do
        conn <- SqlPersistT R.ask
        return $ run conn C.$= massage

      run conn =
        uncurry rawQuery $
        first builderToText $
        toRawSql mode pureQuery conn query

      massage = do
        mrow <- C.await
        case process <$> mrow of
          Just (Right r)  -> C.yield r >> massage
          Just (Left err) -> liftIO $ throwIO $ PersistMarshalError err
          Nothing         -> return ()

      process = sqlSelectProcessRow


-- | Execute an @esqueleto@ @SELECT@ query inside @persistent@'s
-- 'SqlPersistT' monad and return a 'C.Source' of rows.
selectSource :: ( SqlSelect a r
                , MonadLogger m
                , MonadResourceBase m )
             => SqlQuery a
             -> SqlPersistT m (C.Source (C.ResourceT (SqlPersistT m)) r)
selectSource = rawSelectSource SELECT


-- | Execute an @esqueleto@ @SELECT@ query inside @persistent@'s
-- 'SqlPersistT' monad and return a list of rows.
--
-- We've seen that 'from' has some magic about which kinds of
-- things you may bring into scope.  This 'select' function also
-- has some magic for which kinds of things you may bring back to
-- Haskell-land by using @SqlQuery@'s @return@:
--
--  * You may return a @SqlExpr ('Entity' v)@ for an entity @v@
--  (i.e., like the @*@ in SQL), which is then returned to
--  Haskell-land as just @Entity v@.
--
--  * You may return a @SqlExpr (Maybe (Entity v))@ for an entity
--  @v@ that may be @NULL@, which is then returned to
--  Haskell-land as @Maybe (Entity v)@.  Used for @OUTER JOIN@s.
--
--  * You may return a @SqlExpr ('Value' t)@ for a value @t@
--  (i.e., a single column), where @t@ is any instance of
--  'PersistField', which is then returned to Haskell-land as
--  @Value t@.  You may use @Value@ to return projections of an
--  @Entity@ (see @('^.')@ and @('?.')@) or to return any other
--  value calculated on the query (e.g., 'countRows' or
--  'sub_select').
--
-- The @SqlSelect a r@ class has functional dependencies that
-- allow type information to flow both from @a@ to @r@ and
-- vice-versa.  This means that you'll almost never have to give
-- any type signatures for @esqueleto@ queries.  For example, the
-- query @select $ from $ \\p -> return p@ alone is ambiguous, but
-- in the context of
--
-- @
-- do ps <- select $
--          from $ \\p ->
--          return p
--    liftIO $ mapM_ (putStrLn . personName . entityVal) ps
-- @
--
-- we are able to infer from that single @personName . entityVal@
-- function composition that the @p@ inside the query is of type
-- @SqlExpr (Entity Person)@.
select :: ( SqlSelect a r
          , MonadLogger m
          , MonadResourceBase m )
       => SqlQuery a -> SqlPersistT m [r]
select = selectSource >=> runSource


-- | Execute an @esqueleto@ @SELECT DISTINCT@ query inside
-- @persistent@'s 'SqlPersistT' monad and return a 'C.Source' of
-- rows.
selectDistinctSource
  :: ( SqlSelect a r
     , MonadLogger m
     , MonadResourceBase m )
  => SqlQuery a
  -> SqlPersistT m (C.Source (C.ResourceT (SqlPersistT m)) r)
selectDistinctSource = rawSelectSource SELECT_DISTINCT


-- | Execute an @esqueleto@ @SELECT DISTINCT@ query inside
-- @persistent@'s 'SqlPersistT' monad and return a list of rows.
selectDistinct :: ( SqlSelect a r
                  , MonadLogger m
                  , MonadResourceBase m )
               => SqlQuery a -> SqlPersistT m [r]
selectDistinct = selectDistinctSource >=> runSource


-- | (Internal) Run a 'C.Source' of rows.
runSource :: MonadResourceBase m =>
             C.Source (C.ResourceT (SqlPersistT m)) r
          -> SqlPersistT m [r]
runSource src = C.runResourceT $ src C.$$ CL.consume


----------------------------------------------------------------------


-- | (Internal) Execute an @esqueleto@ statement inside
-- @persistent@'s 'SqlPersistT' monad.
rawEsqueleto :: ( MonadLogger m
              , MonadResourceBase m )
           => Mode
           -> SqlQuery ()
           -> SqlPersistT m Int64
rawEsqueleto mode query = do
  conn <- SqlPersistT R.ask
  uncurry rawExecuteCount $
    first builderToText $
    toRawSql mode pureQuery conn query


-- | Execute an @esqueleto@ @DELETE@ query inside @persistent@'s
-- 'SqlPersistT' monad.  Note that currently there are no type
-- checks for statements that should not appear on a @DELETE@
-- query.
--
-- Example of usage:
--
-- @
-- delete $
-- from $ \\appointment ->
-- where_ (appointment ^. AppointmentDate <. val now)
-- @
--
-- Unlike 'select', there is a useful way of using 'delete' that
-- will lead to type ambiguities.  If you want to delete all rows
-- (i.e., no 'where_' clause), you'll have to use a type signature:
--
-- @
-- delete $
-- from $ \\(appointment :: SqlExpr (Entity Appointment)) ->
-- return ()
-- @
delete :: ( MonadLogger m
          , MonadResourceBase m )
       => SqlQuery ()
       -> SqlPersistT m ()
delete = void . deleteCount


-- | Same as 'delete', but returns the number of rows affected.
deleteCount :: ( MonadLogger m
               , MonadResourceBase m )
            => SqlQuery ()
            -> SqlPersistT m Int64
deleteCount = rawEsqueleto DELETE


-- | Execute an @esqueleto@ @UPDATE@ query inside @persistent@'s
-- 'SqlPersistT' monad.  Note that currently there are no type
-- checks for statements that should not appear on a @UPDATE@
-- query.
--
-- Example of usage:
--
-- @
-- update $ \p -> do
-- set p [ PersonAge =. just (val thisYear) -. p ^. PersonBorn ]
-- where_ $ isNull (p ^. PersonAge)
-- @
update :: ( MonadLogger m
          , MonadResourceBase m
          , SqlEntity val )
       => (SqlExpr (Entity val) -> SqlQuery ())
       -> SqlPersistT m ()
update = void . updateCount


-- | Same as 'update', but returns the number of rows affected.
updateCount :: ( MonadLogger m
               , MonadResourceBase m
               , SqlEntity val )
            => (SqlExpr (Entity val) -> SqlQuery ())
            -> SqlPersistT m Int64
updateCount = rawEsqueleto UPDATE . from


----------------------------------------------------------------------


builderToText :: TLB.Builder -> T.Text
builderToText = TL.toStrict . TLB.toLazyTextWith defaultChunkSize
  where
    defaultChunkSize = 1024 - 32


-- | (Internal) Pretty prints a 'SqlQuery' into a SQL query.
--
-- Note: if you're curious about the SQL query being generated by
-- @esqueleto@, instead of manually using this function (which is
-- possible but tedious), you may just turn on query logging of
-- @persistent@.
toRawSql :: SqlSelect a r => Mode -> QueryType a -> Connection -> SqlQuery a -> (TLB.Builder, [PersistValue])
toRawSql mode qt conn query =
  let (ret, SideData fromClauses setClauses whereClauses groupByClause havingClause orderByClauses limitClause) =
        flip S.evalState initialIdentState $
        W.runWriterT $
        unQ query
  in mconcat
      [ makeInsert  qt ret
      , makeSelect  conn mode ret
      , makeFrom    conn mode fromClauses
      , makeSet     conn setClauses
      , makeWhere   conn whereClauses
      , makeGroupBy conn groupByClause
      , makeHaving  conn havingClause
      , makeOrderBy conn orderByClauses
      , makeLimit   conn limitClause
      ]

-- | (Internal) Mode of query being converted by 'toRawSql'.
data Mode = SELECT | SELECT_DISTINCT | DELETE | UPDATE

newtype QueryType a = QueryType { unQueryType :: a -> TLB.Builder }

pureQuery :: QueryType a
pureQuery = QueryType (const mempty)

insertQuery :: PersistEntity a => QueryType (SqlExpr (Insertion a))
insertQuery = QueryType $ \(EInsert p _)->
    let def = entityDef p
        unName = TLB.fromText . unDBName
        fields = uncommas $ map (unName . fieldDB) (entityFields def)
        table = unName . entityDB . entityDef $ p
    in "INSERT INTO " <> table <> parens fields <> "\n"

makeInsert :: QueryType a -> a -> (TLB.Builder, [PersistValue])
makeInsert q a = (unQueryType q a, [])

uncommas :: [TLB.Builder] -> TLB.Builder
uncommas = mconcat . intersperse ", " . filter (/= mempty)

uncommas' :: Monoid a => [(TLB.Builder, a)] -> (TLB.Builder, a)
uncommas' = (uncommas *** mconcat) . unzip


makeSelect :: SqlSelect a r => Connection -> Mode -> a -> (TLB.Builder, [PersistValue])
makeSelect conn mode ret =
  case mode of
    SELECT          -> withCols "SELECT "
    SELECT_DISTINCT -> withCols "SELECT DISTINCT "
    DELETE          -> plain "DELETE "
    UPDATE          -> plain "UPDATE "
  where
    withCols v = first (v <>) (sqlSelectCols conn ret)
    plain    v = (v, [])


makeFrom :: Connection -> Mode -> [FromClause] -> (TLB.Builder, [PersistValue])
makeFrom _    _    [] = mempty
makeFrom conn mode fs = ret
  where
    ret = case collectOnClauses fs of
            Left expr -> throw $ mkExc expr
            Right fs' -> keyword $ uncommas' (map (mk Never mempty) fs')
    keyword = case mode of
                UPDATE -> id
                _      -> first ("\nFROM " <>)

    mk _     onClause (FromStart i def) = base i def <> onClause
    mk paren onClause (FromJoin lhs kind rhs monClause) =
      first (parensM paren) $
      mconcat [ mk Parens onClause lhs
              , (fromKind kind, mempty)
              , mk Never (maybe mempty makeOnClause monClause) rhs
              ]
    mk _ _ (OnClause _) = error "Esqueleto/Sql/makeFrom: never here (is collectOnClauses working?)"

    base ident@(I identText) def =
      let db@(DBName dbText) = entityDB def
      in ( if dbText == identText
           then fromDBName conn db
           else fromDBName conn db <> (" AS " <> useIdent conn ident)
         , mempty )

    fromKind InnerJoinKind      = " INNER JOIN "
    fromKind CrossJoinKind      = " CROSS JOIN "
    fromKind LeftOuterJoinKind  = " LEFT OUTER JOIN "
    fromKind RightOuterJoinKind = " RIGHT OUTER JOIN "
    fromKind FullOuterJoinKind  = " FULL OUTER JOIN "

    makeOnClause (ERaw _ f) = first (" ON " <>) (f conn)

    mkExc :: SqlExpr (Value Bool) -> OnClauseWithoutMatchingJoinException
    mkExc (ERaw _ f) =
      OnClauseWithoutMatchingJoinException $
      TL.unpack $ TLB.toLazyText $ fst (f conn)


makeSet :: Connection -> [SetClause] -> (TLB.Builder, [PersistValue])
makeSet _    [] = mempty
makeSet conn os = first ("\nSET " <>) $ uncommas' (map mk os)
  where
    mk (SetClause (ERaw _ f)) = f conn


makeWhere :: Connection -> WhereClause -> (TLB.Builder, [PersistValue])
makeWhere _    NoWhere            = mempty
makeWhere conn (Where (ERaw _ f)) = first ("\nWHERE " <>) (f conn)


makeGroupBy :: Connection -> GroupByClause -> (TLB.Builder, [PersistValue])
makeGroupBy _ (GroupBy []) = (mempty, [])
makeGroupBy conn (GroupBy fields) = first ("\nGROUP BY " <>) build
  where
    build = uncommas' $ map (\(SomeValue (ERaw _ f)) -> f conn) fields

makeHaving :: Connection -> WhereClause -> (TLB.Builder, [PersistValue])
makeHaving _    NoWhere            = mempty
makeHaving conn (Where (ERaw _ f)) = first ("\nHAVING " <>) (f conn)

makeOrderBy :: Connection -> [OrderByClause] -> (TLB.Builder, [PersistValue])
makeOrderBy _    [] = mempty
makeOrderBy conn os = first ("\nORDER BY " <>) $ uncommas' (map mk os)
  where
    mk (EOrderBy t (ERaw p f)) = first ((<> orderByType t) . parensM p) (f conn)
    orderByType ASC  = " ASC"
    orderByType DESC = " DESC"


makeLimit :: Connection -> LimitClause -> (TLB.Builder, [PersistValue])
makeLimit _    (Limit Nothing Nothing)  = mempty
makeLimit _    (Limit Nothing (Just 0)) = mempty
makeLimit conn (Limit ml      mo)       = (ret, mempty)
  where
    ret = TLB.singleton '\n' <> (limitTLB <> offsetTLB)

    limitTLB =
      case ml of
        Just l  -> "LIMIT " <> TLBI.decimal l
        Nothing -> TLB.fromText (connNoLimit conn)

    offsetTLB =
      case mo of
        Just o  -> " OFFSET " <> TLBI.decimal o
        Nothing -> mempty


parens :: TLB.Builder -> TLB.Builder
parens b = "(" <> (b <> ")")


----------------------------------------------------------------------


-- | (Internal) Class for mapping results coming from 'SqlQuery'
-- into actual results.
--
-- This looks very similar to @RawSql@, and it is!  However,
-- there are some crucial differences and ultimately they're
-- different classes.
class SqlSelect a r | a -> r, r -> a where
  -- | Creates the variable part of the @SELECT@ query and
  -- returns the list of 'PersistValue's that will be given to
  -- 'rawQuery'.
  sqlSelectCols :: Connection -> a -> (TLB.Builder, [PersistValue])

  -- | Number of columns that will be consumed.
  sqlSelectColCount :: Proxy a -> Int

  -- | Transform a row of the result into the data type.
  sqlSelectProcessRow :: [PersistValue] -> Either T.Text r


-- | You may return an insertion of some PersistEntity
instance PersistEntity a => SqlSelect (SqlExpr (Insertion a)) (Insertion a) where
  sqlSelectCols esc (EInsert _ f) = let (b, vals) = f esc
                                    in (b, vals)
  sqlSelectColCount = const 0
  sqlSelectProcessRow = const (Right Proxy)


-- | Not useful for 'select', but used for 'update' and 'delete'.
instance SqlSelect () () where
  sqlSelectCols _ _ = ("1", [])
  sqlSelectColCount _ = 1
  sqlSelectProcessRow _ = Right ()


-- | You may return an 'Entity' from a 'select' query.
instance PersistEntity a => SqlSelect (SqlExpr (Entity a)) (Entity a) where
  sqlSelectCols conn expr@(EEntity ident) = ret
      where
        process ed = uncommas $
                     map ((name <>) . fromDBName conn) $
                     (entityID ed:) $
                     map fieldDB $
                     entityFields ed
        -- 'name' is the biggest difference between 'RawSql' and
        -- 'SqlSelect'.  We automatically create names for tables
        -- (since it's not the user who's writing the FROM
        -- clause), while 'rawSql' assumes that it's just the
        -- name of the table (which doesn't allow self-joins, for
        -- example).
        name = useIdent conn ident <> "."
        ret = let ed = entityDef $ getEntityVal $ return expr
              in (process ed, mempty)
  sqlSelectColCount = (+1) . length . entityFields . entityDef . getEntityVal
  sqlSelectProcessRow (idCol:ent) =
    Entity <$> fromPersistValue idCol
           <*> fromPersistValues ent
  sqlSelectProcessRow _ = Left "SqlSelect (Entity a): wrong number of columns."

getEntityVal :: Proxy (SqlExpr (Entity a)) -> Proxy a
getEntityVal = const Proxy


-- | You may return a possibly-@NULL@ 'Entity' from a 'select' query.
instance PersistEntity a => SqlSelect (SqlExpr (Maybe (Entity a))) (Maybe (Entity a)) where
  sqlSelectCols conn (EMaybe ent) = sqlSelectCols conn ent
  sqlSelectColCount = sqlSelectColCount . fromEMaybe
    where
      fromEMaybe :: Proxy (SqlExpr (Maybe e)) -> Proxy (SqlExpr e)
      fromEMaybe = const Proxy
  sqlSelectProcessRow cols
    | all (== PersistNull) cols = return Nothing
    | otherwise                 = Just <$> sqlSelectProcessRow cols


-- | You may return any single value (i.e. a single column) from
-- a 'select' query.
instance PersistField a => SqlSelect (SqlExpr (Value a)) (Value a) where
  sqlSelectCols esc (ERaw p f) = let (b, vals) = f esc
                                 in (parensM p b, vals)
  sqlSelectColCount = const 1
  sqlSelectProcessRow [pv] = Value <$> fromPersistValue pv
  sqlSelectProcessRow _    = Left "SqlSelect (Value a): wrong number of columns."


-- | You may return tuples (up to 16-tuples) and tuples of tuples
-- from a 'select' query.
instance ( SqlSelect a ra
         , SqlSelect b rb
         ) => SqlSelect (a, b) (ra, rb) where
  sqlSelectCols esc (a, b) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      ]
  sqlSelectColCount = uncurry (+) . (sqlSelectColCount *** sqlSelectColCount) . fromTuple
    where
      fromTuple :: Proxy (a,b) -> (Proxy a, Proxy b)
      fromTuple = const (Proxy, Proxy)
  sqlSelectProcessRow =
    let x = getType processRow
        getType :: SqlSelect a r => (z -> Either y (r,x)) -> Proxy a
        getType = const Proxy

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
  sqlSelectColCount   = sqlSelectColCount . from3P
  sqlSelectProcessRow = fmap to3 . sqlSelectProcessRow

from3P :: Proxy (a,b,c) -> Proxy ((a,b),c)
from3P = const Proxy

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
  sqlSelectColCount   = sqlSelectColCount . from4P
  sqlSelectProcessRow = fmap to4 . sqlSelectProcessRow

from4P :: Proxy (a,b,c,d) -> Proxy ((a,b),(c,d))
from4P = const Proxy

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
  sqlSelectColCount   = sqlSelectColCount . from5P
  sqlSelectProcessRow = fmap to5 . sqlSelectProcessRow

from5P :: Proxy (a,b,c,d,e) -> Proxy ((a,b),(c,d),e)
from5P = const Proxy

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
  sqlSelectColCount   = sqlSelectColCount . from6P
  sqlSelectProcessRow = fmap to6 . sqlSelectProcessRow

from6P :: Proxy (a,b,c,d,e,f) -> Proxy ((a,b),(c,d),(e,f))
from6P = const Proxy

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
  sqlSelectColCount   = sqlSelectColCount . from7P
  sqlSelectProcessRow = fmap to7 . sqlSelectProcessRow

from7P :: Proxy (a,b,c,d,e,f,g) -> Proxy ((a,b),(c,d),(e,f),g)
from7P = const Proxy

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
  sqlSelectColCount   = sqlSelectColCount . from8P
  sqlSelectProcessRow = fmap to8 . sqlSelectProcessRow

from8P :: Proxy (a,b,c,d,e,f,g,h) -> Proxy ((a,b),(c,d),(e,f),(g,h))
from8P = const Proxy

to8 :: ((a,b),(c,d),(e,f),(g,h)) -> (a,b,c,d,e,f,g,h)
to8 ((a,b),(c,d),(e,f),(g,h)) = (a,b,c,d,e,f,g,h)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         ) => SqlSelect (a, b, c, d, e, f, g, h, i) (ra, rb, rc, rd, re, rf, rg, rh, ri) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      ]
  sqlSelectColCount   = sqlSelectColCount . from9P
  sqlSelectProcessRow = fmap to9 . sqlSelectProcessRow

from9P :: Proxy (a,b,c,d,e,f,g,h,i) -> Proxy ((a,b),(c,d),(e,f),(g,h),i)
from9P = const Proxy

to9 :: ((a,b),(c,d),(e,f),(g,h),i) -> (a,b,c,d,e,f,g,h,i)
to9 ((a,b),(c,d),(e,f),(g,h),i) = (a,b,c,d,e,f,g,h,i)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      ]
  sqlSelectColCount   = sqlSelectColCount . from10P
  sqlSelectProcessRow = fmap to10 . sqlSelectProcessRow

from10P :: Proxy (a,b,c,d,e,f,g,h,i,j) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j))
from10P = const Proxy

to10 :: ((a,b),(c,d),(e,f),(g,h),(i,j)) -> (a,b,c,d,e,f,g,h,i,j)
to10 ((a,b),(c,d),(e,f),(g,h),(i,j)) = (a,b,c,d,e,f,g,h,i,j)


instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      ]
  sqlSelectColCount   = sqlSelectColCount . from11P
  sqlSelectProcessRow = fmap to11 . sqlSelectProcessRow

from11P :: Proxy (a,b,c,d,e,f,g,h,i,j,k) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),k)
from11P = const Proxy

to11 :: ((a,b),(c,d),(e,f),(g,h),(i,j),k) -> (a,b,c,d,e,f,g,h,i,j,k)
to11 ((a,b),(c,d),(e,f),(g,h),(i,j),k) = (a,b,c,d,e,f,g,h,i,j,k)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         , SqlSelect l rl
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k, l) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k, l) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      , sqlSelectCols esc l
      ]
  sqlSelectColCount   = sqlSelectColCount . from12P
  sqlSelectProcessRow = fmap to12 . sqlSelectProcessRow

from12P :: Proxy (a,b,c,d,e,f,g,h,i,j,k,l) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l))
from12P = const Proxy

to12 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l)) -> (a,b,c,d,e,f,g,h,i,j,k,l)
to12 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l)) = (a,b,c,d,e,f,g,h,i,j,k,l)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         , SqlSelect l rl
         , SqlSelect m rm
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k, l, m) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k, l, m) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      , sqlSelectCols esc l
      , sqlSelectCols esc m
      ]
  sqlSelectColCount   = sqlSelectColCount . from13P
  sqlSelectProcessRow = fmap to13 . sqlSelectProcessRow

from13P :: Proxy (a,b,c,d,e,f,g,h,i,j,k,l,m) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m)
from13P = const Proxy

to13 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m) -> (a,b,c,d,e,f,g,h,i,j,k,l,m)
to13 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m) = (a,b,c,d,e,f,g,h,i,j,k,l,m)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         , SqlSelect l rl
         , SqlSelect m rm
         , SqlSelect n rn
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k, l, m, n) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      , sqlSelectCols esc l
      , sqlSelectCols esc m
      , sqlSelectCols esc n
      ]
  sqlSelectColCount   = sqlSelectColCount . from14P
  sqlSelectProcessRow = fmap to14 . sqlSelectProcessRow

from14P :: Proxy (a,b,c,d,e,f,g,h,i,j,k,l,m,n) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n))
from14P = const Proxy

to14 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
to14 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         , SqlSelect l rl
         , SqlSelect m rm
         , SqlSelect n rn
         , SqlSelect o ro
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      , sqlSelectCols esc l
      , sqlSelectCols esc m
      , sqlSelectCols esc n
      , sqlSelectCols esc o
      ]
  sqlSelectColCount   = sqlSelectColCount . from15P
  sqlSelectProcessRow = fmap to15 . sqlSelectProcessRow

from15P :: Proxy (a,b,c,d,e,f,g,h,i,j,k,l,m,n, o) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o)
from15P = const Proxy

to15 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
to15 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

instance ( SqlSelect a ra
         , SqlSelect b rb
         , SqlSelect c rc
         , SqlSelect d rd
         , SqlSelect e re
         , SqlSelect f rf
         , SqlSelect g rg
         , SqlSelect h rh
         , SqlSelect i ri
         , SqlSelect j rj
         , SqlSelect k rk
         , SqlSelect l rl
         , SqlSelect m rm
         , SqlSelect n rn
         , SqlSelect o ro
         , SqlSelect p rp
         ) => SqlSelect (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) (ra, rb, rc, rd, re, rf, rg, rh, ri, rj, rk, rl, rm, rn, ro, rp) where
  sqlSelectCols esc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =
    uncommas'
      [ sqlSelectCols esc a
      , sqlSelectCols esc b
      , sqlSelectCols esc c
      , sqlSelectCols esc d
      , sqlSelectCols esc e
      , sqlSelectCols esc f
      , sqlSelectCols esc g
      , sqlSelectCols esc h
      , sqlSelectCols esc i
      , sqlSelectCols esc j
      , sqlSelectCols esc k
      , sqlSelectCols esc l
      , sqlSelectCols esc m
      , sqlSelectCols esc n
      , sqlSelectCols esc o
      , sqlSelectCols esc p
      ]
  sqlSelectColCount   = sqlSelectColCount . from16P
  sqlSelectProcessRow = fmap to16 . sqlSelectProcessRow

from16P :: Proxy (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) -> Proxy ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p))
from16P = const Proxy

to16 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
to16 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

-- | Apply a 'PersistField' constructor to @SqlExpr Value@ arguments
(<#) :: (a -> b) -> SqlExpr (Value a) -> SqlExpr (Insertion b)
(<#) _ (ERaw _ f) = EInsert Proxy f

-- | Apply extra @SqlExpr Value@ arguments to a 'PersistField' constructor
(<&>) :: SqlExpr (Insertion (a -> b)) -> SqlExpr (Value a) -> SqlExpr (Insertion b)
(EInsert _ f) <&> (ERaw _ g) = EInsert Proxy $ \x->
  let (fb, fv) = f x
      (gb, gv) = g x
  in (fb <> ", " <> gb, fv ++ gv)

-- | Insert a 'PersistField' for every selected value
insertSelect :: (MonadLogger m, MonadResourceBase m, SqlSelect (SqlExpr (Insertion a)) r, PersistEntity a) =>
  SqlQuery (SqlExpr (Insertion a)) -> SqlPersistT m ()
insertSelect = insertGeneralSelect SELECT

-- | Insert a 'PersistField' for every unique selected value
insertSelectDistinct :: (MonadLogger m, MonadResourceBase m, SqlSelect (SqlExpr (Insertion a)) r, PersistEntity a) =>
  SqlQuery (SqlExpr (Insertion a)) -> SqlPersistT m ()
insertSelectDistinct = insertGeneralSelect SELECT_DISTINCT


insertGeneralSelect :: (MonadLogger m, MonadResourceBase m, SqlSelect (SqlExpr (Insertion a)) r, PersistEntity a) =>
  Mode -> SqlQuery (SqlExpr (Insertion a)) -> SqlPersistT m ()
insertGeneralSelect mode query = do
  conn <- SqlPersistT R.ask
  uncurry rawExecute $ first builderToText $ toRawSql mode insertQuery conn query
